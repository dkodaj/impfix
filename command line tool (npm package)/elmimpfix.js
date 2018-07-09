#!/usr/bin/env node

var chalk = require('chalk');
var elm = require('./Impworker.js');
var elmWorker = elm.ImpWorker.worker();
var fs = require('fs');
var glob = require('glob');
var parsePath = require('parse-filepath');
var program = require('commander');

var exclude = [];

elmWorker.ports.error.subscribe(errLog);

elmWorker.ports.output.subscribe( (fixed) => {
	var outputFlag = program.output;
	var replace = program.replace;
	if (!outputFlag && !replace) outputFlag = "./*_Impfix.*";
	if (replace) outputFlag = "**/*.*";
	var out = outputFileName(fixed.name, outputFlag);
	fs.writeFile(out, fixed.code, (err) => {
  		if (err) {
  			errLog(err);
  		} else {
  			console.log(chalk.bold.green('Impfix dun! '), fixed.name, chalk.bold.green(' >> '), out);
  		}
	});
});

function errLog(err){
	if (err) console.log(chalk.bold.cyan('Impfix '+err));
}

function list(flag) {
  return flag.split(' ');
}

function normalizeNewlines(txt){
	return txt.replace(new RegExp("\r\n", 'g'), "\n");
}

function noWildcards(txt){
	return (txt.indexOf('*') === -1)
}

function outputFileName(source, outputFlag) {
	var sourceParsed = parsePath(source);
	if (outputFlag) {
		outParsed = parsePath(outputFlag);
		var outDir = outParsed.dir.replace("**", sourceParsed.dir);
		var outStem = outParsed.stem.replace("*", sourceParsed.stem);
		var outExt = outParsed.ext.replace("*", sourceParsed.ext.replace('.',''));
		return outDir + "/" + outStem + outExt;
	} else {
		return sourceParsed.dir + "/" + sourceParsed.stem + "_Impfix" + sourceParsed.ext;
	}
}

function sendImportFile(filename){
	try {
		const data = fs.readFileSync(filename,'utf8');
		elmWorker.ports.deliverImport.send(normalizeNewlines(data));
	} catch(err) {
		errLog(err);
	}
}

function sendSourceFile(filename){
	if (exclude.indexOf(filename) > -1) return;
	try {
		if (filename.indexOf("/")===-1) filename = "./"+filename;
		const data = fs.readFileSync(filename,'utf8');
		var file = {};
    	file.name = filename;
    	file.code = normalizeNewlines(data);
    	elmWorker.ports.deliverSource.send(file);
	} catch(err) {
		errLog(err);
	}
}

program
	.name('elm-impfix')
	.usage('<filename> [options]')	
	.arguments('<file(s)>', 'Name of file to fix')
	.option('-o, --output <file(s)>','Output file (default: name_Impfix.ext)')
	.option('-r, --replace','Replace all affected files (equivalent to -o "**/*.*")')
	.option('-x, --exclude <file(s)>','Don\' touch these files (e.g "-x ./elm-stuff/*.*")')
	.option('-q, --qualify <file(s)>','Source code of unqualified imports (optional)', list)
	.version('1.0.6','-v, --version')
	.action(function(source){
		if (program.exclude) exclude = glob.sync(program.exclude, {absolute:true});
		if (program.qualify) {
			for (var qualifySource of program.qualify) {
				if (noWildcards(qualifySource)) {
					sendImportFile(qualifySource);
				} else {
					try {
						const files = glob.sync(qualifySource, {absolute:true});
						if (files) {
							for (var file of files) {
								sendImportFile(file);
							}
						}
					} catch(err) {
						errLog(err);
					}
				}
			}
		}
		if (noWildcards(source)){
			sendSourceFile(source);
		} else {
			try {
				const files = glob.sync(source, {absolute:true});
				if (files) {
					for (var file of files) {
						sendSourceFile(file);
					}
				}
			} catch(err) {
				errLog(err);
			}
		}
	});

program.on('--help', function(){
	  console.log('');
	  console.log('  Notes:');
	  console.log('  -----');
	  console.log('  • All parameters support wildcards. Wildcard params must be quoted.');
	  console.log('  • -q qualifies "exposing (...)" or "exposing(MyType(...))" style imports.');
	})

program.parse(process.argv);

