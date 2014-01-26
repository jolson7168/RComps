#!/bin/bash

# This script, invoked with the --user-data-file switch on ec2 instance creation time, will execute as root after the instance is created.
# Upon completion, the image will have your S3 bucket mounted at /data and the RComps project mounted at /code


# Create the password file to mount S3 bucket
echo <S3 key here> >>/etc/passwd-s3fs

# Create a mount point for the S3 bucket
mkdir /data

# Change owner to ubuntu on mount point
chown ubuntu /data

# Load the fuse driver. This is the S3 driver.
modprobe fuse

# Mount the S3 bucket, allowing access
/usr/local/bin/s3fs -o allow_other <bucket name here> /data
echo "S3 data mounted as /data"

# Create a mount point for the project repository
mkdir /code

# Change ownership of mount point
chown ubuntu /code

# Pull the project source code down 
git clone https://github.com/jo5729/RComps.git /code
