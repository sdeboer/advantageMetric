App = angular.module 'Advantage'

class SummaryController
	constructor: (@$scope, conf)->
		# NOP at the moment

App.controller 'SummaryController', ['$scope', 'Advantage.conf', SummaryController]

