dt=`date +%y%m%d%H%M%S`
R CMD BATCH /src/RComps/src/script_ec2.R /src/RComps/script_ec2.Rout
mv /src/RComps/script_ec2.Rout /data/script_ec2.Rout.$dt
mv /src/RComps/R_test_output.dat /data/R_test_output.dat.$dt

