#!/usr/bin/env python3

import sys, os, subprocess
import time

HERBIE_DIR="/home/hbecker/Git_Repos/herbie"
FPBENCH_DIR="/home/hbecker/Git_Repos/FPBench"
DAISY_DIR="/home/hbecker/Git_Repos/daisy"

# EXPERIMENTS_DIR="/home/hbecker/Git_Repos/daisy/testcases/cpp2018"
EXPERIMENTS_DIR="/home/hbecker/Git_Repos/FPBench/benchmarks"


def getFiles():
    fileObj = open(EXPERIMENTS_DIR + "/daisy_herbie.fpcore", "r")
    fileContent = fileObj.read()
    benchmarks = fileContent.split("\n\n")
    return benchmarks
    # fileC = ""
    # fileIndex = []
    # for dirname, subdirs, files in os.walk(EXPERIMENTS_DIR):
    #     for file in files:
    #         suffix=file.split(".")[1]
    #         if suffix == "fpcore":
    #             filep = open (dirname + "/" + file, "r")
    #             for line in filep:
    #                 fileC = fileC + line
    #             fileIndex.append (dirname + "/" + file)
    return [] #fileIndex

def runFilter (fileC) :
    infile =open("/tmp/filter_in.txt", "w")
    infile.write(fileC)
    infile.flush()
    infile.close()
    infile=open("/tmp/filter_in.txt", "r")
    progRet = subprocess.Popen(["racket",
                                FPBENCH_DIR + "/tools/filter.rkt",
                                "operations",
                                "+ - / '*'"],
                               stdout=subprocess.PIPE,
                               stdin=infile)
    progRet.wait()
    print(progRet.stdout.read().decode())

# run Herbies improvement phase on inFname file,
# produce resulting fpcore file in outFname
def runHerbie (inFname, outFname) :
    start = time.time()
    resultText=""
    progRet = subprocess.Popen (["racket",
                                HERBIE_DIR + "/src/herbie.rkt",
                                "improve",
                                inFname,
                                outFname],
                                stdout=subprocess.PIPE)
    progRet.wait()
    end = time.time()
    stdoutBinary = progRet.stdout.read()
    stdoutText = stdoutBinary.decode()
    print(stdoutText)
    return (end-start, progRet.returncode)

# Run FPCore2Scala converter on file inFname, write output to file outFname
def runConverter (inFname, outFname):
    infile = open (inFname, "r")
    outfile = open (outFname, "w")
    proc = subprocess.Popen (
        ["racket",
         FPBENCH_DIR + "/tools/core2scala.rkt"],
        stdin=infile,
        stdout=outfile)
    proc.wait()
    outfile.flush()
    infile.close()
    outfile.close()
    return proc.returncode

def runDaisy (inFname,certificates=True):
    cwd = os.getcwd()
    os.chdir(DAISY_DIR)
    outfile = open ("/tmp/daisy_res.txt","w")
    errfile = open ("/tmp/daisy_err.txt", "w")
    start = time.time()
    proc = subprocess.Popen (["./daisy",
                              inFname],
                             stdout= outfile,
                             stderr = errfile)
    proc.wait()
    end = time.time()

    outfile.flush()
    errfile.flush()
    outfile.close()
    errfile.close()
    os.chdir(cwd)

    runTime = end - start

    resFile = open ("/tmp/daisy_res.txt", "r")
    for line in resFile:
        if "abs-error" in line:
            return (runTime,line.split(",")[0].split(":")[1])

    return (runTime, "FAILED")

# test runner wrapper function,
# iterate over all files in given list, run the evaluation pipeline
# store all results in output file as csv table, to parse externally
def runTests (benchmarks,tableName):
    table = open(tableName, "w")
    table.write("Benchmark name, Herbie , Daisy (src), Daisy (res), roundoff err (src), roundoff err (res)")
    table.write("\n")
    for benchmark in benchmarks:
        if not benchmark:
            continue
        afterName=benchmark.split(":name")[1]
        theName=benchmark.split('"')[1]
        table.write(theName + ", ")
        input=open("/tmp/herbie_in.fpcore","w")
        input.write(benchmark)
        input.close()
        (timeHerbie, exitcode) = runHerbie ("/tmp/herbie_in.fpcore", "/tmp/herbie_out.fpcore")
        if not exitcode == 0:
            table.write("Herbie FAILED , FAILED, FAILED, FAILED, FAILED\n")
            continue
        table.write (str(round(timeHerbie,2)) + " ,")
        retInConv = runConverter ("/tmp/herbie_in.fpcore", "/tmp/herbie_in.scala")
        if not retInConv == 0:
            table.write("InputConversion Failed, InputConversion Failed, InputConversion Failed, InputConversion Failed\n")
            continue
        retOutConv = runConverter("/tmp/herbie_out.fpcore", "/tmp/herbie_out.scala")
        (timeInDaisy, errSrcDaisy) = runDaisy ("/tmp/herbie_in.scala")
        if not retOutConv == 0:
            table.write(str(round(timeInDaisy,2)) + ", OutputConversion Failed, " + errSrcDaisy + ", OutputConversion Failed\n")
            continue
        (timeOutDaisy, errOutDaisy) = runDaisy ("/tmp/herbie_out.scala")
        table.write(str(round(timeInDaisy,2)) + ", " + str(round(timeOutDaisy,2)) + ", " + errSrcDaisy +", " + errOutDaisy + "\n")
    table.close()
    return
    #for file in files:
    #theName = getProperName("/home/hbecker/Git_Repos/FPBench/benchmarks/doppler1.fpcore")
    #table.write(theName)
    #timeHerbie = runHerbie ("/home/hbecker/Git_Repos/FPBench/benchmarks/doppler1.fpcore",
    #"/tmp/out_herbie1.fpcore")
    #runConverter ("/home/hbecker/Git_Repos/FPBench/benchmarks/herbie.fpcore",
    #"/tmp/in_herbie.scala")
    # runConverter("/tmp/out_herbie1.fpcore", "/tmp/out_herbie.scala")
    # (timeInDaisy, errSrcDaisy) = runDaisy ("/tmp/in_herbie.scala")
    # (timeOutDaisy, errOutDaisy) = runDaisy ("/tmp/out_herbie.scala")
    # table.write(str(timeInDaisy) + ", " + str(timeOutDaisy) + ", " + errSrcDaisy + errOutDaisy)
    # table.close()
    return

def main(argv):
    print ("Starting to get files")
    benchmarks=getFiles()
    runTests(benchmarks, "/home/hbecker/Documents/herbie_daisy_prelim_results.csv")
    return 0

### UTILITY functions
def getProperName (fNameWithPath):
    fnameWithoutPathList = fNameWithPath.split("/")
    fNameNoPath = fnameWithoutPathList[len(fnameWithoutPathList)-1]
    return (fNameNoPath.split(".")[0]) #remove file extension


if __name__ == "__main__":
    main(sys.argv)
