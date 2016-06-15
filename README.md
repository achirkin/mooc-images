## mooc-images
=================================================

A small web service based on yesod.
Developed to support edX courses provided by the Chair iA at ETH Zurich.
Provides two pages:

1. `/share` - a post-form to be embedded into edX for uploading images;
2. `/` - view images uploaded by students.


##### Native dependencies:

Uses [`GD`](http://libgd.github.io/) library for making image previews,
available on Ubuntu via
> apt-get install libgd-dev

Needs a PostgreSQL server and client libraries if one uses this database,
available on Ubuntu via
> apt-get install postgresql libpq-dev


#### Using yesod toolset for development and deployment

This requires `yesod-bin` haskell package that is available in hackage and stackage.

Deploy mooc-images to a configured keter server:
```
yesod keter
```

Run mooc-images in development mode:
```
yesod devel
```

Run test suite:
```
yesod test
```


#### location-import

Command-line tool to upload locations from [geonames.org](http://www.geonames.org/).
Need to run this tools at least once before running the web service;
otherwise, users won't be able upload any images! 


#### prevcourses-import

Command-line tool to upload image from exported edX discussions



#### Many thanks

For the design template: [Daemonite's Material UI](https://github.com/Daemonite/material).

For the geographical data: [geonames.org](http://www.geonames.org/).

For the web server: [yesodweb](http://www.yesodweb.com/).
