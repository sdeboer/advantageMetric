App = angular.module 'Advantage'

class SummonerController
	constructor: (@$scope, @Summoner, conf)->
		@$scope.active_regions = conf.active_regions

		@$scope.a =
			summoner_name: null
			region: conf.active_regions[0]
			submit: @submitFormA
		@$scope.b =
			summoner_name: null
			region: conf.active_regions[0]
			submit: @submitFormB

		@$scope.result = {}
		@$scope.result.data = []

	submitFormA: =>
		@$scope.result_a = @Summoner.get {summoner_name: @$scope.a.summoner_name}, @responseA

	submitFormB: =>
		@$scope.result_b = @Summoner.get {summoner_name: @$scope.b.summoner_name}, @responseB

	responseA: =>
		@response @$scope.result_a

	responseB: =>
		@response @$scope.result_b

	response: (result)=>
		if @$scope.result_a?.$resolved and @$scope.result_b?.$resolved
			@setCompare()

		requestAnimationFrame =>
			for g in result.summoner
				@setSeries g

	setCompare: ->
		asum = @$scope.result_a.summary
		bsum = @$scope.result_b.summary
		a = (asum.count - bsum.count)
		@$scope.result_a.count_advantage = @advantage a
		@$scope.result_a.summary.count_compare = a

		a = (asum.average - bsum.average)
		@$scope.result_a.summary.average_compare = a
		@$scope.result_a.summary.average_advantage = @advantage a

		a = (asum.sdev - bsum.sdev)
		@$scope.result_a.summary.sdev_compare = a
		@$scope.result_a.summary.sdev_advantage = @advantage a


	advantage: (a)->
		a >= 0 ? 'positive' : 'negative'

	setSeries: (game)->
		# blue is position 0, purple is position 11,
		# players are their participantId
		series = []
		totals = []
		blue = name: "Blue", data: [], color: 'blue', yAxis: 1
		Purple = name: "Purple", data: [], color: 'purple', yAxis: 1
		winner = if game.match.teams[0].winner
			"Blue"
		else
			"Purple"
		
		for streaks in game.streaks
			for s in streaks
				x = s.start * 60 * 1000
				pts = s.score

				if s.team is 100
					current = totals[0] = (totals[0] || 0) + pts
					blue.data.push [x, totals[0]]
				else
					current = totals[11] = (totals[11] || 0) + pts
					purple.data.push [x, totals[11]]

				for pid in s.players when pid isnt 0
					totals[pid] = (totals[pid] || 0) + pts
					percent = (totals[pid] / current) * 100.0
					point = [x, percent]

					if series[pid]?
						series[pid].data.push point
					else
						name = game.match.participantIdentities[pid - 1].player.summonerName
						if game.pid is pid
							champ_name = name
							champ_team_pos = if s.team is 100
								0
							else
								11
							visibility = true
						else
							visibility = false

						series[pid] = visible: visibility, name: name, data: [ point ]

		series[0] = blue
		series[11] = purple

		champ_score = Math.round((totals[game.pid] / totals[champ_team_pos]) * 100)

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
			subtitle: { text: "#{champ_name} : #{champ_score}% / #{winner} team wins" }
			chart: { type: 'spline' }
			title: { text: 'Team Advantage'}
			xAxis: { title: { text: 'Time'}, type: 'datetime', dateTimeLabelFormats: formats }
			yAxis: [
				{
					title: { text: 'Player Contribution (%)'}
					gridLineWidth: 0
					min: 0
					max: 100
				},
				{
					title: { text: 'Team Score'}
					opposite: true
					min: 0
				}
			]
			plotOptions: {
				spline: { marker: { enabled: true } }
			},
			series: series
		}

		$(".section .streak[data-match-id='#{game.match.matchId}'").highcharts(options)

App.controller 'SummonerController', ['$scope', 'SummonerResource', 'Advantage.conf', SummonerController]

