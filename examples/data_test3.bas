' Test DATA/READ/RESTORE with implicit variables
DATA 10, 20, 30, 3.14159

READ a%, b%, c%, pi#

PRINT "Values: "; a%; b%; c%
PRINT "Pi: "; pi#

RESTORE
READ a%
PRINT "After RESTORE: "; a%

END
