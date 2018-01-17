function initMap() {
  var map;
  var bounds = new google.maps.LatLngBounds();
  var mapOptions = {
      mapTypeId: 'roadmap'
  };
                  
  map = new google.maps.Map(document.getElementById("map_canvas"), mapOptions);
  map.setTilt(45);
      
  var markers = [
      ['New York University', 40.729423, -73.997214],
      ['University of California Davis', 38.539875, -121.754869],
      ['University of Michigan', 42.276254, -83.741244],
      ['Montana State University', 45.666756, -111.04981],
      ['University of Texas San Antonio', 29.584344, -98.617356],
      ['McGill University', 45.506167, -73.577642],
      ['University of the Repulic', -34.902596, -56.176535],
      ['The University Paris 8', 48.944877, 2.362956],
      ['Hong Kong University of Science and Technology', 22.336400, 114.265466]
  ];
                      
  var infoWindowContent = [
      ['<div class="info_content">New York University</div>'],
      ['<div class="info_content">University of California Davis</div>'],
      ['<div class="info_content">University of Michigan</div>'],
      ['<div class="info_content">Montana State University</div>'],
      ['<div class="info_content">University of Texas San Antonio</div>'],
      ['<div class="info_content">McGill University</div>'],
      ['<div class="info_content">The University Paris 8</div>'],
      ['<div class="info_content">Hong Kong University of Science and Technology</div>']
  ];
      
  var infoWindow = new google.maps.InfoWindow(), marker, i;
    
  for( i = 0; i < markers.length; i++ ) {
      var position = new google.maps.LatLng(markers[i][1], markers[i][2]);
      bounds.extend(position);
      marker = new google.maps.Marker({
          position: position,
          map: map,
          title: markers[i][0]
      });
          
      google.maps.event.addListener(marker, 'mouseover', (function(marker, i) {
          return function() {
              infoWindow.setContent(infoWindowContent[i][0]);
              infoWindow.open(map, marker);
          }
      })(marker, i));

      map.fitBounds(bounds);
  }

  var boundsListener = google.maps.event.addListener((map), 'bounds_changed', function(event) {
      this.setZoom(3);
      google.maps.event.removeListener(boundsListener);
  });
    
}