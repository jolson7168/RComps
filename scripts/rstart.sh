# This script will launch the R instance on EC2.

ec2-run-instances ami-e42cdb8d --instance-count 1 --key <key pair> --group <security group> --instance-type <instance size> --user-data-file <path-to-startup>/startup.sh --availability-zone us-east-1a --monitor --instance-initiated-shutdown-behavior terminate
