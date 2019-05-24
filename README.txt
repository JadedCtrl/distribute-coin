===============================================================================
DISTRIBUTE-COIN                                              Seize the tokens ♥
===============================================================================

distribute-coin's a privacy-focused  image-uploading and URL-shortening
service. It doesn't have bells and whistles like accounts or cookies, 
JS or tracking… which you might consider good things.
I think they're awful.

Probably the one bell it has is that it's backed up by IPFS. Whenever you
upload a file, it's 'added' to IPFS and pinned to the gateway. Then, a 
shortened URL (containing the file-name you chose/came with the file) is
created that redirects to the actual IPFS URL.


----------------------------------------
USAGE
----------------------------------------
Just use `quicklisp` (pop this in your "~/quicklisp/local-projects/",
and you're good).
There is one thing that isn't in quicklisp that this needs, though:
	* cl-ipfs-api² (https://git.eunichx.us/cl-ipfs-api2.git)

Once you've loaded :distribute-coin into your REPL, just run:
	(distribute-coin:server)

You might wanna look at the docstrings for more information, esp.
on usage of #'server.


----------------------------------------
BORING STUFF
----------------------------------------
License is AGPLv3-- check COPYING.txt.
Author is Jaidyn Ann <jadedctrl@teknik.io>
Permanent instance over at https://coinsh.red
Sauce is at https://git.eunichx.us/distribute-coin.git
