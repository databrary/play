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
      link: function($scope) {
        var form, volume;
        volume = $scope.volume;
        form = $scope.metadataMatchForm;
        form.save = function() {
          messages.clear(form);
          var data = { "csv_upload_id": volume.csv_upload_id, "selected_mapping":[{ "metric": volume.suggested_mapping[0].metric, "csv_field": volume.suggested_mapping[0].csv_field}]};
          form.$setSubmitted();
          return volume.matchcsv(data).then(function() {
            $('metadata-match-form').hide();
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
      }
    };
  }
]);
