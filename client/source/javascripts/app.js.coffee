app = (router, location, http)->
	http.defaults.useXDomain = true
	http.defaults.withCredentials = true
	delete http.defaults.headers.common['X-Requested-With']

	router.when('/summoner/',
		templateUrl: '/templates/summoner.html')

	router.when('/layout/',
		templateUrl: '/templates/layout.html')

	router.otherwise redirectTo: '/summoner'

angular.module 'Advantage',
	['ngResource', 'ngRoute'],
	['$routeProvider', '$locationProvider', '$httpProvider', app]
