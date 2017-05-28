# Distributed-LSH

The run the recalltest (of Hyperplane) in single machine mode issue the following command:

sbt "project RecallTest" "run --data sampleFiles/128-float.txt --dataeuc sampleFiles/128-float.txt --size 100 --setup single --dimensions 128 --dataType numeric --nodesAddresses sampleFiles/nodesaddresses.txt --testCases sampleFiles/testcases-euc-nondistributed.txt --invocationCount 10 --warmUpIterations 2000 --out data/out --seed 95731014"

run the following command for the description of the flags:

sbt "project RecallTest" "run --help"

