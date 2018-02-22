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
              body: constants.message('volume.edit.success'),
              owner: form
            });
            form.$setPristine();
          }, function(res) {
            form.$setUnsubmitted();
            form.validator.server(res);
            messages.addError({
              body: constants.message('volume.edit.error'),
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
      link: function($scope) {
        var form, volume;
        volume = $scope.volume;
        form = $scope.metadataMatchForm;
        
        form.save = function() {
          var data;
          messages.clear(form);
          console.log(volume.suggested_mapping);
          data = new FormData();
          data.append('csv_upload_id', volume.csv_upload_id);
          var json_arr = JSON.stringify([{ "metric": "id", "csv_field": "col1"}])
          data.append('selected_mapping', json_arr);
          console.log(data);
          form.$setSubmitted();
          return volume.matchcsv(data).then(function() {
            form.validator.server({});
            messages.add({
              type: 'green',
              body: constants.message('volume.edit.success'),
              owner: form
            });
            form.$setPristine();
          }, function(res) {
            form.$setUnsubmitted();
            form.validator.server(res);
            messages.addError({
              body: constants.message('volume.edit.error'),
              report: res,
              owner: form
            });
          });
        };
      }
    };
  }
]);
