export SHELL=/bin/bash
export HOME=/home/hadoop
export PROJECT_HOME=/home/hadoop/hlivy
export S3_HOME=s3://earnest-development/donohue/hlivy_dev

software:
	-curl -sSL https://get.haskellstack.org/ | sh
	-echo "export PATH=${HOME}/.local/bin:${PATH}" >> ${HOME}/.bashrc
	-sudo yum install -y gmp-devel.x86_64 git jq

local-cache:
	aws s3 cp ${S3_HOME}/cache.tar.gz ${HOME}
	cd ${HOME}; tar -xzvf cache.tar.gz

remote-cache:
	cd ${HOME}; tar -czf cache.tar.gz .stack hlivy/.stack-work
	aws s3 cp ${HOME}/cache.tar.gz ${S3_HOME}/

project:
	aws s3 cp --recursive ${S3_HOME}/.ssh/ ${HOME}/.ssh/
	chmod 600 ${HOME}/.ssh/id_rsa ${HOME}/.ssh/authorized_keys
	git clone git@github.com:EarnestResearch/hlivy.git
	cd ${PROJECT_HOME}; git checkout develop

devtools:
	-sudo yum install -y emacs
	aws s3 cp ${S3_HOME}/.emacs ${HOME}
	aws s3 cp ${S3_HOME}/.ghci ${HOME}
	sudo chmod go-w ${HOME}/.ghci
	-stack install hlint stylish-haskell

setup: software project local-cache devtools

teardown: remote-cache
