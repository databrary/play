'use strict';
app.directive('metadataForm', [
  'constantService', 'messageService', function(constants, messages) {
    return {
      restrict: 'E',
      templateUrl: 'volume/metadata.html',
      link: function($scope) {
        var form, volume;
        volume = $scope.volume;
        form = $scope.metadataForm;
        
        form.save = function() {
          var data;
          messages.clear(form);
          data = new FormData();
          data.append('file', form.data.metadata[0]);
          form.$setSubmitted();
          return volume.detectcsv(data).then(function() {
            $('metadata-form').hide();
            $('metadata-match-form').show();
            form.validator.server({});
            messages.add({
              type: 'green',
              body: constants.message('volume.metadatadetect.success'),
              owner: form
            });
            form.$setPristine();
            volume.selected_mapping = volume.suggested_mapping;
          }, function(res) {
            form.$setUnsubmitted();
            form.validator.server(res);
            messages.addError({
              body: constants.message('volume.metadatadetect.error'),
              report: res,
              owner: form
            });
          });
        };
      }
    };
  }
]);

app.directive('metadataMatchForm', [
  'constantService', 'messageService', function(constants, messages) {
    return {
      restrict: 'E',
      templateUrl: 'volume/metadatamatch.html',
      require: 'ngModel',
      link: function($scope, ctrl) {
        var form, volume;
        volume = $scope.volume;
        form = $scope.metadataMatchForm;
        form.save = function() {
          messages.clear(form);
          var data = { "csv_upload_id": volume.csv_upload_id, "selected_mapping": volume.selected_mapping};
          form.$setSubmitted();
          return volume.matchcsv(data).then(function() {
            form.validator.server({});
            messages.add({
              type: 'green',
              body: constants.message('volume.metadataupload.success'),
              owner: form
            });
            form.$setPristine();
          }, function(res) {
            form.$setUnsubmitted();
            form.validator.server(res);
            messages.addError({
              body: constants.message('volume.metadataupload.error'),
              report: res,
              owner: form
            });
          });
        };
        $scope.skip = function(metric){
          $('.skip-text').show();
          $('.repeat-card > *').not('.skip-text').hide();
          volume.selected_mapping = volume.selected_mapping.filter(function( obj ) {
            return obj.metric !== metric;
          });
          form.save();
        }
        $scope.edit = function(){
          $('.skip-text').hide();
          $('.repeat-card > *').not('.skip-text').show();
        }
      }
    };
  }
]);
