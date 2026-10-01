
This is a version of astorb that uses mmapped-table instead of building a fasl.  It has the advantage of having a universal database file, and not 
putting the whole database into RAM at once, plus building faster.  It is a tiny bit slower.

