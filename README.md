#### mooc-images

A small web service based on yesod.
Developed to support edX courses provided by the Chair iA at ETH Zurich.
Provides two pages:

1. `/share` - a post-form to be embedded into edX for uploading images;
2. `/` - view images uploaded by students.



#### location-import

Command-line tool to upload locations from [geonames.org](http://www.geonames.org/).
Need to run this tools at least once before running the web service;
otherwise, users won't be able upload any images! 

##### Dependencies:

Uses [`GD`](http://libgd.github.io/) library for making image previews,
available on Ubuntu via
> apt-get install libgd-dev

Needs a PostgreSQL server and client libraries if one uses this database,
available on Ubuntu via
> apt-get install postgresql libpq-dev

