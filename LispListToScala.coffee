class LispListToScala

	constructor: (origVal) ->
		renderedVal = this.addCons this.addNils this.addParenthesis origVal
		console.log(renderedVal)

	addParenthesis: (toVal) ->
		r = /(\b\w+)/gm
		return toVal.replace(r, '"$&"')

	addNils: (toVal) ->
		r = /(\))/gm
		return toVal.replace(r, ' Nil)')

	addCons: (toVal) ->
		r = /([ |\n])/gm
		return toVal.replace(r, ' ::$&')


fs = require('fs')
fs.readFile(process.argv.pop(), 'utf8', (err,data) ->
	if (err)
		return console.log(err);
	new LispListToScala(data) 
)

