#!/usr/bin/env python3

import sys, os, subprocess, tempfile, time

# Set by running the script
HERBIE_DIR = DAISY_DIR = FPBENCH_DIR = None

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
    return out.stdout.split("\n\n")

# run Herbies improvement phase on inFname file,
# produce resulting fpcore file in outFname
def runHerbie (benchmark) :
    start = time.time()
    out = subprocess.run(
        ["racket", HERBIE_DIR + "/src/herbie.rkt", "improve", "-", "-"],
        stdout=subprocess.PIPE,
        input=benchmark, universal_newlines=True)
    try:
        result = out.stdout.split("\n")[-2]
    except IndexError:
        result = "ERROR"
    dt = time.time() - start
    return dt, result, out.returncode

# Run FPCore2Scala converter on file inFname, write output to file outFname
def runConverter (benchmark):
    out = subprocess.run(
        ["racket", FPBENCH_DIR + "/tools/core2scala.rkt"],
        input=benchmark, universal_newlines=True,
        stdout=subprocess.PIPE)
    return out.stdout, out.returncode

def runDaisy (benchmark, certificates=True):
    cwd = os.getcwd()
    os.chdir(DAISY_DIR)
    start = time.time()
    with tempfile.NamedTemporaryFile(suffix=".scala") as f:
        f.write(benchmark.encode("utf-8"))
        f.flush()
        out = subprocess.run(
            ["./daisy", f.name],
            stdout=subprocess.PIPE, universal_newlines=True)
    dt = time.time() - start
    os.chdir(cwd)

    if out.returncode:
        print(out.stdout, file=sys.stderr)
        print(out.stderr, file=sys.stderr)
        return dt, "FAILED", out.returncode

    for line in out.stdout.split("\n"):
        if "abs-error" in line:
            return dt, float(line.split(",")[0].split(":")[1]), out.returncode
    else:
        print(out.stderr, file=sys.stderr)
        print(out.stdout, file=sys.stderr)
        return dt, "FAILED", out.returncode

def runTest(in_fpcore):
    if ":name" in in_fpcore:
        name = in_fpcore.split(":name")[1].split('"')[1]
    else:
        name = in_fpcore
    yield name

    (timeHerbie, out_fpcore, exitcode) = runHerbie (in_fpcore)
    if not exitcode == 0:
        print("HERBIE ERROR ON: ", in_fpcore, file=sys.stderr)
        return
    yield timeHerbie

    (in_scala, exitcode) = runConverter (in_fpcore)
    if not exitcode == 0:
        print("CONVERTER ERROR ON: ", in_fpcore, file=sys.stderr)
        return

    (out_scala, exitcode) = runConverter (out_fpcore)
    if not exitcode == 0:
        print("CONVERTER ERROR ON: ", out_fpcore, file=sys.stderr)
        return

    (timeInDaisy, errInDaisy, exitCode) = runDaisy (in_scala)
    if not exitcode == 0:
        print("DAISY ERROR ON: ", in_scala, file=sys.stderr)
        return

    (timeOutDaisy, errOutDaisy, exitCode) = runDaisy (out_scala)
    if not exitcode == 0:
        print("DAISY ERROR ON: ", out_scala, file=sys.stderr)
        return

    yield from [timeInDaisy, timeOutDaisy, errInDaisy, errOutDaisy]

def runTests(benchmarks):
    print("Benchmark name, Herbie, Daisy (src), Daisy (res), roundoff err (src), roundoff err (res)")
    for benchmark in benchmarks:
        first = True
        for field in runTest(benchmark):
            text = '"{}"'.format(field.replace("\"", "\\\"")) if isinstance(field, str) else "{:.2g}".format(field)
            print(text if first else ", " + text, end="")
            sys.stdout.flush()
            first = False
        print()

def main():
    benchmarks = list(getFiles())
    print ("Found {} total benchmarks".format(len(benchmarks)), file=sys.stderr)
    benchmarks = runFilter(benchmarks, ["operations", "+", "-", "/", "*"])
    benchmarks = runFilter(benchmarks, ["pre"])
    print ("Filtered down to {} benchmarks".format(len(benchmarks)), file=sys.stderr)
    runTests(benchmarks)

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("herbie_dir", help="The directory of the Herbie source code", type=dir_type)
    parser.add_argument("daisy_dir", help="The directory of the Daisy source code", type=dir_type)
    args = parser.parse_args()

    HERBIE_DIR = args.herbie_dir
    DAISY_DIR = args.daisy_dir
    current_dir = os.path.dirname(__file__)
    FPBENCH_DIR = os.path.dirname(dir_type(current_dir or "."))

    main()
