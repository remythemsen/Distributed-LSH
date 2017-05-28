# Distributed-LSH

## Hyperplane based recall test

To run the recalltest (of Hyperplane) in single machine mode issue the following command:

sbt "project RecallTest" "run --data sample_files/128-float.txt --dataeuc sample_files/128-float.txt --size 100 --setup single --dimensions 128 --dataType numeric --nodesAddresses sample_files/nodesaddresses.txt --testCases sample_files/testcases-euc-nondistributed.txt --invocationCount 10 --warmUpIterations 2000 --out . --seed 95731014"

## BitHash based recall test

To run the recalltest with BitHash in single machine mode issue the following command:

sbt "project RecallTest" "run --data sample_files/256-binary.txt --dataeuc sample_files/256-binary.txt --size 100 --setup single --dimensions 256 --dataType binary --nodesAddresses sample_files/nodesaddresses.txt --testCases sample_files/testcases-bin-nondistributed.txt --invocationCount 10 --warmUpIterations 2000 --out . --seed 59731014"


## NOTE:
The datasets provides in sample_files folder are very small and will not yield good results with the settings described above.
They are merely included to show the format the parser will accept. The settings in the sample_files testcases fits a dataset of size: ~500.000



run the following command for the description of the flags:

sbt "project RecallTest" "run --help"
