define(['app/config/module'], function (module) {
	'use strict';

	module.directive('browserList', ['BrowserService', '$filter', 'ConstantService', 'RouterService', function (browserService, $filter, constantService, routerService) {
		var link = function ($scope, $element, $attrs) {
			if (!$scope.browser)
				$scope.browser = browserService;

			if (!$scope.router)
				$scope.router = routerService;

			if (!$scope.constant)
				$scope.constant = constantService;

			$scope.getInclude = function () {
				if (!$scope.data.items[0])
					return '';

				switch ($scope.data.items[0].group) {
					case 'volume':
						return 'browserVolume.html';

					case 'session':
						return 'browserSession.html';

					case 'asset':
						return 'browserAsset.html';

					default:
						return 'browserRecord.html';
				}
			};

			$scope.toggleExpand = function () {
				$scope.data = $scope.browser.setItemExpand($scope.data);
			};

			$scope.expanderClasses = function (data) {
				var classes = [];

				classes.push(data.expand ? 'active' : '');

				return classes;
			};

			$scope.itemClasses = function (data) {
				var classes = [];

				if (!data.expand)
					classes.push('deepest');

				return classes;
			};

			$scope.setItemSelect = function (data) {
				$scope.browser.setItemSelect(data);
			};

			//

			$scope.setItemPlayer = function (data) {
				$scope.browser.setItemPlayer(data);
			};

			//

			$scope.getName = function (data) {
				switch ($scope.browser.getItemType(data.object)) {
					case 'volume':
						return data.object.name;

					case 'record':
						var category = $scope.constant.data.category[data.object.category].name;
						return category.charAt(0).toUpperCase() + category.slice(1) + ': ' + (data.object.measures.ident || data.object.id);

					case 'session':
						return 'Session: ' + (data.object.name || data.object.id);
				}
			};

			$scope.formatAge = function (age) {
				return $filter('age')(age);
			};

			$scope.formatSessionCategory = function (data, categoryID) {
				var category = $scope.constant.get('category', categoryID);

				if (!category)
					return 'Uncategorized';

				switch (category.name) {
					default:
						return category.name.charAt(0).toUpperCase() + category.name.slice(1) + 's';
				}
			};

			$scope.capitalize = function (input) {
				return input.charAt(0).toUpperCase() + input.slice(1);
			};

			$scope.nameRecord = function (data) {
				var category = $scope.constant.get('category', data.object.category),
					name;
				console.log(category.id);
				if (data.object.id == 0) {
					switch (category.id) {
						case -800:
							name = 'Not pilot';
							break;

						case -700:
							name = 'Included';
							break;

						case -500:
							name = 'No participants';
							break;

						case -400:
							name = 'No conditions';
							break;

						case -200:
							name = 'Not grouped';
							break;

						case -100:
							name = 'No location';
							break;

						default:
							name = 'No ' + category.name;
							break;
					}
				} else {
					name = $scope.capitalize($scope.constant.get('category', data.object.category).name);
				}
				
				if (data.object.id != 0)
					switch (category.id) {
						case -700:
							name += ': ' + data.object.measures.reason;
							break;

						case -100:
							name += ': ' + data.object.measures.setting;
							break;

						case -400:
						case -200:
						case -500:
							name += ': ' + data.object.measures.ident;
							break;
					}

				return name;
			};

			//

			$scope.editLink = function (data) {
				switch ($scope.browser.getItemType(data.object)) {
					case 'volume':
						return $scope.router.volumeEdit(data.object);

					case 'record':
						return $scope.router.recordEdit(data.object);

					case 'session':
						return $scope.router.slotEdit(data.object);

					case 'asset':
						return $scope.router.assetEdit(data.object);
				}
			};
		};

		return {
			restrict: 'E',
			scope: {
				'data': '='
			},
			templateUrl: 'browserList.html',
			replace: true,
			priority: 100,
			link: link
		};
	}]);
});
