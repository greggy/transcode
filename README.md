## Transcode Project


### Overview

To say trully, the name 'transcode' is not correct for the project. Its
main goal is to read tasks from AWS S3, check there metadata and from it
make deсision what to do. Task is an empty file with some information in
metadata.

We use it as a bridge between web application and server application. Web
put some data into the task and server get it to work with local files via
sh script.


### Dependencies

To build transcode you will need a working installation of Erlang R14B03 
(or later) and:

 * erlcloud
 * lager

To get them just run:

    > make get-deps


### Download

    > git clone http://github.com/greggy/tcpproxy.git


### Configuration

All configurations are stored in ./rel/files/sys.config file. Section 'transcode'.

 * s3_auth: AWS S3 authentication information;
 * bucket: AWS S3 bucket;
 * script_name: which script will proceed whith the task;
 * num_tries: how many times a worker will work with task; 
 * node_id: the unique node name (every node will work only with own tasks);


### Instalation

    > cd ./transcode
    > make get-deps
    > ./mkrel.sh

Transcode is compiled and started!
