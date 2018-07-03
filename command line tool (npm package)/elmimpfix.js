#!/usr/bin/env node

var chalk = require('chalk');
var elm = require('./Impworker.js');
var elmWorker = elm.ImpWorker.worker();
var fs = require('fs');
var glob = require('glob');
var parsePath = require('parse-filepath');
var program = require('commander');

elmWorker.ports.error.subscribe(errLog);

elmWorker.ports.output.subscribe( (fixed) => {
	var outputFlag = program.output;
	if (!outputFlag) outputFlag = "*_Impfix.*";
    if (outputFlag.indexOf("/") === -1 && outputFlag.indexOf("\\") === -1) {
		outputFlag = "./" + outputFlag; 
	}
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

function outputFileName(source,output) {
	sourceParsed = parsePath(source);
	if (output) {
		outParsed = parsePath(output);
		var outStem = outParsed.stem.replace("*", sourceParsed.stem);
		var outExt = outParsed.ext.replace("*", sourceParsed.ext.replace('.',''));
		return unixify(outParsed.dir) + "/" + outStem + outExt;
	} else {
		return unixify(sourceParsed.dir) + "/" + sourceParsed.stem + "_Impfix" + sourceParsed.ext;
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
	try {
		const data = fs.readFileSync(filename,'utf8');
		var file = {};
    	file.name = filename;
    	file.code = normalizeNewlines(data);
    	elmWorker.ports.deliverSource.send(file);
	} catch(err) {
		errLog(err);
	}
}

function unixify(path){
	return path.replace(new RegExp("\\\\", 'g'), "/");
}

program
	.name('elm-impfix')
	.usage('<filename> [options]')	
	.arguments('<filename>', 'Name of file to fix')
	.option('-o, --output <filename>','Output file (default: name_Impfix.ext)')
	.option('-q, --qualify <filename(s)>','Source code of unqualified imports (optional)', list)
	.version('1.0.0','-v, --version')
	.action(function(source){
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
	  console.log('  • <filename> supports wildcards. Use relative paths.');
	  console.log('  • -o supports wildcards in filename but not in path.');
	  console.log('  • -q can be used to qualify "exposing (...)" or "exposing(MyType(...))" style imports.');
	  console.log('  • -q supports wildcards. Quote lists of files (-q "module1.elm module2.elm").');
	})

program.parse(process.argv);

