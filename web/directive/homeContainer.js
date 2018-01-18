'use strict';
app.directive('homeContainer', [
  '$document', function($document) {
    return {
      restrict: 'A',
      link: function($scope, $element, $attrs) {
        //arrow on click slide to the next section
        $('.arrow-container').click(function(){
          $('html, body').animate({
                scrollTop: $(this).closest('.section').next('.section').find('h2').offset().top - 50
            }, 1000);
        });
        //nyu slider
        var streamWidth = 0;
        $('.stream li').each(function(){
          streamWidth += parseInt($(this).width(), 10);
        });
        $(".stream-nav.left").click(function () { 
            var maxWidth = streamWidth + 10 - window.innerWidth;
            var windowWidth = window.innerWidth - 80;
            $('.stream li').each(function(){
              $(this).css('margin-left', Math.min((parseInt($(this).css('margin-left'), 10) + windowWidth), 0) + 'px');
              $(this).css('margin-right', Math.max((parseInt($(this).css('margin-right'), 10) - windowWidth), 0) + 'px');
            });
        });

        $(".stream-nav.right").click(function () {
          var maxWidth = streamWidth + 10 - window.innerWidth;
          var windowWidth = window.innerWidth - 80;
          $('.stream li').each(function(){
            $(this).css('margin-left', Math.max((parseInt($(this).css('margin-left'), 10) - windowWidth), -Math.abs(maxWidth)) + 'px');
            $(this).css('margin-right', Math.min((parseInt($(this).css('margin-right'), 10) + windowWidth), maxWidth) + 'px');
          });
        });

        //stop video if the user plays another video
        $('video').each(function(){
          $(this).on('play', function(){
            $('video').not($(this)).each(function(){
              $(this).get(0).pause();
            });
          });
        });

        //testimonial carousel
        var myIndex = 0;
        var stopCarousel = false;

        function carousel() {
            var i;
            var x = document.getElementsByClassName("testimonial");
            for (i = 0; i < x.length; i++) {
               x[i].style.display = "none";  
            }
            myIndex++;
            if (myIndex > x.length) {myIndex = 1}    
            x[myIndex-1].style.display = "block"; 
            if (stopCarousel === false) {
              setTimeout(carousel, 8000);
            }
        }

        carousel();

        var slideIndex = 1;

        function showDivs(n) {
          var i;
          var x = document.getElementsByClassName("testimonial");
          if (n > x.length) {slideIndex = 1} 
          if (n < 1) {slideIndex = x.length} ;
          for (i = 0; i < x.length; i++) {
              x[i].style.display = "none"; 
          }
          x[slideIndex-1].style.display = "block"; 
        }

        showDivs(slideIndex);

        $scope.plusDivs = function(n) {
            showDivs(slideIndex += n);
            stopCarousel = true;
        }

        jQuery(function($) {
            // Asynchronously Load the map API 
            var script = document.createElement('script');
            script.src = "https://maps.googleapis.com/maps/api/js?key=AIzaSyBZRvCMW10DAK5WKF9QCgtNWdmhkwpGinc&callback=initMap";
            document.body.appendChild(script);
        });
      }
    };
  }
]);
