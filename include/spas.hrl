-define (packageKey (PackageName, BundleID),
	<<BundleID/binary, "^", PackageName/binary>>).

-define (GET, <<"spashttp.request">>).