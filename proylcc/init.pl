
:- module(init, [ init/3 ]).

init(
	[[2], [2,3], [1,2], [2,4], [7], [5,2], [9], [2,5], [2,2], [2]],	% PistasFilas
	

	[[1,1], [3,3], [5], [1,3], [2,5,1], [9], [8], [2,2], [4], [2]], 	% PistasColumnas
	

	[["X", _ , _ , "#" , _ , _ ,"X", _ , _ , _ ],
	[_ , "#" , _ , "X" , "#", _ ,"#", _ ,"X", _],
	["X","#" , _ , _ , _ ,"#" , _ , _ , _  , _ ],
	[_ , _ , "#" , "X", _ , _ , _ , _ , "X", _ ],
	["X", _ , _ ,"#", "#","#","#","#", _  , _  ],
	[_ , _ , _ , _ , _ , _ , _ , _ , _  , "#"  ],
	[_ , _ , _ , _ ,"#","#","#", "#", _  , _   ],
	[_ , _ ,"#","X", _ , _ ,"#", _ ,"#", "X"   ],
	["#","#", _ , _ , _ , _ , _ , "X", _  ,"X" ],
	[_ ,"X", _ , _ , _  ,"#","X", _ , _  , "X" ]
	]
	).
