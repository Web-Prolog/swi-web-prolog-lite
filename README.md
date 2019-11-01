# swi-web-prolog-lite

The code in this repo is just a hack to allow us to experiment with the caching mechanism supporting the stateless web API. It likely isn't worth expanding. To create a node, run

```
$ swipl node.pl
```

and then direct your browser to http://localhost:3010 .

It is possible to run nodes on different hosts other than localhost.  To set the hostname, edit node.pl and change all instances of 'localhost' to the desired hostname.  Also, you will need to modify shell.html and, again, replace all instances of localhost.
