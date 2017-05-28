# Distributed-LSH

The recall test run in single machine mode by issuing the following command:

sbt "project RecallTest" "run --data data/profiset-128-float.data --dataeuc data/profiset-128-float.data --size 20171529 --setup distributed --dimensions 128 --dataType numeric --nodesAddresses data/ips --testCases data/testcases-torun-mp --invocationCount 1 --warmUpIterations 2000 --out data/out --seed 95731014"

run the following command for the description of the flags:

sbt "project RecallTest" "run --help"

