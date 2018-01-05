#!/usr/bin/env python3

import sys, os, subprocess, tempfile, time, shlex

# Set by running the script
SAVE_DIR = HERBIE_DIR = DAISY_DIR = FPBENCH_DIR = None
HERBIE_FLAGS = ""
DAISY_FLAGS = ""

def dir_type(path):
    if not os.path.exists(path):
        raise argparse.ArgumentTypeError("Path does not exist", path)
    if not os.path.isdir(path):
        raise argparse.ArgumentTypeError("Path is not a directory", path)
    return os.path.realpath(os.path.expanduser(path))

def getFiles():
    for dirname, subdirs, files in os.walk(FPBENCH_DIR + "/benchmarks"):
        for file in files:
            if not file.endswith(".fpcore"): continue

            with open(dirname + "/" + file, "r") as f:
                yield from filter(lambda x: x, f.read().split("\n\n"))

def runFilter (benchmarks, args) :
    out = subprocess.run(
        ["racket", FPBENCH_DIR + "/tools/filter.rkt"] + args,
        input="\n\n".join(benchmarks), universal_newlines=True,
        stdout=subprocess.PIPE)
    return out.stdout.strip().split("\n\n")

def addPreconditions (sexp_file, benchmarks) :
    pres = {}
    for line in sexp_file:
        # TODO: Very hacky
        name = line.split("(")[1].strip()
        precondition = line.rstrip()[1+len(name):-1].strip()
        assert name[0] == name[-1] == '"', "Failed to parse the extra preconditions file"
        pres[name] = precondition

    used = {}
    for benchmark in benchmarks:
        for name, pre in pres.items():
            if ":name " + name in benchmark or ":name\n " + name in benchmark:
                break
        else:
            yield benchmark
            continue
        used[name] = True
        idx = benchmark.index(")") + 1
        yield benchmark[:idx] + " :pre " + pre + " " + benchmark[idx:]

    for name, pre in pres.items():
        if name not in used:
            print("Unused extra precondition `{}` (possible misspelled name)".format(name))

def parse_herbie_error(err):
    assert err.startswith("((") and err.endswith("))")
    nums = [(int(x.split(" ", 1)[0]), float(x.split(" ", 1)[1])) for x in err[2:-2].split(") (")]
    return max(nums)[1]

# run Herbies improvement phase on inFname file,
# produce resulting fpcore file in outFname
def runHerbie (benchmark, timeout=300) :
    start = time.time()
    out = subprocess.run(
        ["racket", HERBIE_DIR + "/src/herbie.rkt", "improve", "--timeout", str(timeout)] + HERBIE_FLAGS + ["-", "-"],
        stdout=subprocess.PIPE,
        input=benchmark, universal_newlines=True)
    dt = time.time() - start

    try:
        if out.stdout.count("\n") > 1:
            result = out.stdout.split("\n")[-2]
        else:
            return dt, "ERROR", "ERROR", "ERROR", (out.returncode or 1)
        fields = {x.split(" ", 1)[0]: x.split(" ", 1)[1].strip() for x in result.split(":")[1:-1]}
        if "herbie-error-input" in fields and "herbie-error-output" in fields:
            start_error = parse_herbie_error(fields["herbie-error-input"])
            end_error = parse_herbie_error(fields["herbie-error-output"])
            return dt, result, start_error, end_error, out.returncode
        else:
            return dt, result, "TIMEOUT", "TIMEOUT", out.returncode
    except:
        import traceback
        traceback.print_exc()
        return dt, "ERROR", "ERROR", "ERROR", (out.returncode or 1)

# Run FPCore2Scala converter on file inFname, write output to file outFname
def runConverter (benchmark):
    out = subprocess.run(
        ["racket", FPBENCH_DIR + "/tools/core2scala.rkt"],
        input=benchmark, universal_newlines=True,
        stdout=subprocess.PIPE)
    return out.stdout, out.returncode

def runDaisy (benchmark, timeout=300):
    csv_out = os.path.join(DAISY_DIR, "output", "d2h.csv")
    if os.path.exists(csv_out): os.remove(csv_out)
    cwd = os.getcwd()
    os.chdir(DAISY_DIR)
    start = time.time()
    with tempfile.NamedTemporaryFile(suffix=".scala") as f:
        f.write(benchmark.encode("utf-8"))
        f.flush()
        out = subprocess.run(
            ["timeout", "{}s".format(timeout), "./daisy", "--results-csv=d2h.csv"] + DAISY_FLAGS + [f.name],
            stdout=subprocess.PIPE, universal_newlines=True)
    dt = time.time() - start
    os.chdir(cwd)

    if out.returncode or not os.path.exists(csv_out):
        if "Zero denominator not allowed" in out.stdout:
            error = "DIV0.A"
        elif "trying to divide by interval containing 0" in out.stdout:
            error = "DIV0.B"
        elif "error: not found: value " in out.stdout:
            error = "FN." + out.stdout.split("error: not found: value ", 1)[1].split(" ", 1)[0]
        elif "Power is only supported for positive integer powers > 2" in out.stdout:
            error = "POW"
        elif out.returncode == 124 or out.returncode == 137: # see timeout(1)
            error = "TIMEOUT"
        else:
            error = "FAILED"
        print("DAISY ERROR ", error, " FOR ", DAISY_FLAGS, file=sys.stderr, flush=True)
        print(out.stdout, file=sys.stderr)
        return dt, error, (out.returncode or 1)

    print (out.stdout, file=sys.stdout)

    with open(csv_out) as f:
        csvdata = f.read()
        if csvdata.count("\n") < 1:
            return dt, "NOOUT", (out.returncode or 1)
        try:
            abs_err = float(csvdata.split("\n")[1].rsplit(",", 4)[1])
            return dt, abs_err, out.returncode
        except:
            import traceback
            traceback.print_exc()
            return dt, "EXCEPTION", (out.returncode or 1)

def runTest(idx, in_fpcore, args):
    if ":name" in in_fpcore:
        name = in_fpcore.split(":name")[1].split('"')[1]
    else:
        name = in_fpcore
    yield name

    if SAVE_DIR: open(os.path.join(SAVE_DIR, idx + ".input.fpcore"), "wt").write(in_fpcore)

    (timeHerbie, out_fpcore, in_err, out_err, exitcode) = runHerbie (in_fpcore, timeout=args.timeout)
    if not exitcode == 0:
        print("HERBIE ERROR ON: ", in_fpcore, file=sys.stderr, flush=True)
        return
    yield from [timeHerbie, in_err, out_err]

    (in_scala, exitcode) = runConverter (in_fpcore)
    if SAVE_DIR: open(os.path.join(SAVE_DIR, idx + ".output.fpcore"), "wt").write(out_fpcore)

    if not exitcode == 0:
        print("CONVERTER ERROR ON: ", in_fpcore, file=sys.stderr, flush=True)
        return

    (out_scala, exitcode) = runConverter (out_fpcore)
    if SAVE_DIR: open(os.path.join(SAVE_DIR, idx + ".input.scala"), "wt").write(in_scala)

    if not exitcode == 0:
        print("CONVERTER ERROR ON: ", out_fpcore, file=sys.stderr, flush=True)
        return

    (timeInDaisy, errInDaisy, exitcode) = runDaisy (in_scala, timeout=args.timeout)
    if SAVE_DIR: open(os.path.join(SAVE_DIR, idx + ".output.scala"), "wt").write(out_scala)

    (timeOutDaisy, errOutDaisy, exitcode) = runDaisy (out_scala, timeout=args.timeout)

    yield from [timeInDaisy, timeOutDaisy, errInDaisy, errOutDaisy]

def runTests(benchmarks, args):
    cols = [ "Index"
           , "Name"
           , "Herbie time"
           , "Herbie src error"
           , "Herbie res error"
           , "Daisy src time"
           , "Daisy res time"
           , "Daisy src error"
           , "Daisy res error"
           ]
    print('"' + '", "'.join(cols) + '"')
    for idx, benchmark in enumerate(benchmarks):
        print(idx, end=",")
        sys.stdout.flush()
        first = True
        for field in runTest(str(idx), benchmark, args):
            if isinstance(field, str):
                field = field.replace(',', '')
                field = field.replace('"', '\\"')
                text = '"{}"'.format(field)
            else:
                text = "{:0.3g}".format(field)
            print(text if first else ", " + text, end="")
            sys.stdout.flush()
            first = False
        print()

def main(args):
    benchmarks = list(getFiles())
    print ("Found {} total benchmarks".format(len(benchmarks)), file=sys.stderr)
    if args.extra_preconditions:
        benchmarks = list(addPreconditions(args.extra_preconditions, benchmarks))
    benchmarks = runFilter(benchmarks, ["operations", "+", "-", "/", "*", "exp", "log", "sin", "cos", "let", "sqrt", "tan"])
    benchmarks = runFilter(benchmarks, ["pre"])
    print ("Filtered down to {} benchmarks".format(len(benchmarks)), file=sys.stderr)
    runTests(benchmarks, args)

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--save", help="The directory to save intermediate files into", type=dir_type)
    parser.add_argument("herbie_dir", help="The directory of the Herbie source code", type=dir_type)
    parser.add_argument("daisy_dir", help="The directory of the Daisy source code", type=dir_type)
    parser.add_argument("--daisy-flags", help="Flags to pass to Daisy", type=str, default="")
    parser.add_argument("--herbie-flags", help="Flags to pass to Herbie", type=str, default="")
    parser.add_argument("--extra-preconditions", help="An S-expression file of new preconditions to apply", type=open, default=None)
    parser.add_argument("--timeout", help="How many seconds to time out at for Herbie and Daisy", type=int, default=300)
    args = parser.parse_args()

    HERBIE_DIR = args.herbie_dir
    DAISY_DIR = args.daisy_dir
    HERBIE_FLAGS = shlex.split(args.herbie_flags)
    DAISY_FLAGS = shlex.split(args.daisy_flags)
    current_dir = os.path.dirname(__file__)
    FPBENCH_DIR = os.path.dirname(dir_type(current_dir or "."))
    if args.save: SAVE_DIR = args.save

    main(args)
