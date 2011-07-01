
open Libgrew
open Log

let _ = Log.set_active_levels [`MESSAGE;`INFO;`DEBUG]

let grs_file = "/media/DATA/grew/resources/depling/depling.grs"
let gr_file = "/media/DATA/grew/resources/depling/exemples/ex1_Jean_s_en_souvient.gr"

let _ = 
	try
		let grs = Libgrew.grs grs_file in
		Log.fmessage "The file '%s' has been loaded as Graph Rewrinting System" grs_file;
		let gr = Libgrew.gr gr_file in
		Log.fmessage "The file '%s' has been loaded as Graph" gr_file;
		
		let rew = Libgrew.rewrite ~gr ~grs ~mock:true in
		()
		
	with Libgrew.File_dont_exists file -> Log.fcritical "The file '%s' doesn't exist!!\n%!" file
