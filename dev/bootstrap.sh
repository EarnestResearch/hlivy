#!/bin/bash
set -e

HOME=/home/hadoop
S3_HOME=s3://earnest-development/donohue/hlivy_dev
aws s3 cp $S3_HOME/Makefile $HOME
sudo su - hadoop -c "make setup"
