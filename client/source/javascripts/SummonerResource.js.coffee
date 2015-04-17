actions =
	get: { method: 'JSONP', params: {jsonp: 'JSON_CALLBACK'} }

Summoner = ($resource, host)->
	$resource host + "/summoner/:summoner_name",
		null,
		actions

angular.module('Advantage').factory 'SummonerResource', ['$resource', 'MasterUrl', Summoner]
