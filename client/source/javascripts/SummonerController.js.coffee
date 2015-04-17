App = angular.module 'Advantage'

class SummonerController
	constructor: (@$scope, @Summoner, conf)->
		@$scope.active_regions = conf.active_regions

		@$scope.base =
			summoner_name: null
			region: conf.active_regions[0]

		@$scope.result = {}
		@$scope.result.data = []

		@$scope.base.submit = @submitForm

	submitForm: =>
		console.log 'alterForm', @$scope.base.summoner_name, @$scope.base.region
		@$scope.result = @Summoner.get {summoner_name: @$scope.base.summoner_name}, @response

	response: =>
		console.log 'response', @$scope.result.base

		requestAnimationFrame =>
			for g in @$scope.result.base
				@setSeries g

	setSeries: (game)->
		# blue is position 0, red is position 11,
		# players are their participantId
		series = []
		totals = []
		blue = name: "Blue", data: [], color: 'blue'
		red = name: "Red", data: [], color: 'red'
		
		for streaks in game.streaks
			for s in streaks
				x = s.start * 60 * 1000
				pts = s.score

				if s.team is 100
					totals[0] = (totals[0] || 0) + pts
					blue.data.push [x, totals[0]]
				else
					totals[11] = (totals[11] || 0) + pts
					red.data.push [x, totals[11]]

				for pid in s.players when pid isnt 0
					totals[pid] = (totals[pid] || 0) + pts

					if series[pid]?
						series[pid].data.push [x, totals[pid]]
					else
						series[pid] = visible: false, name: "Player #{pid}", data: [ [x, totals[pid]] ]

		series[0] = blue
		series[11] = red
		formats =
			millisecond: '%H:%M:%S'
			second: '%M:%S'
			minute: '%M:%S'
			hour: '%H:%M:%S'
			day: '%H:%M:%S'
			week: '%H:%M:%S'
			month: '%H:%M:%S'
			year: '%H:%M:%S'

		options = {
			tooltip: { dateTimeLabelFormats: formats }
			chart: { type: 'spline' }
			title: { text: 'Team Advantage'}
			xAxis: { title: { text: 'Time'}, type: 'datetime', dateTimeLabelFormats: formats }
			yAxis: { title: { text: 'Score'} }
			plotOptions: {
				spline: { marker: { enabled: true } }
			},
			series: series
		}

		$(".summary .streak[data-match-id='#{game.match.matchId}'").highcharts(options)

App.controller 'SummonerController', ['$scope', 'SummonerResource', 'Advantage.conf', SummonerController]

