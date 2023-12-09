(function($) {
	
	"use strict";


    // Back to top
    $.scrollUp({
        scrollText: '<i class="fas fa-sort-up"></i>',
        easingType: 'linear',
        scrollSpeed: 900,
        animation: 'fade'
    });
     
    // Bootstrap Tooltip
    $(function () {
        $('[data-toggle="tooltip"]').tooltip()
    });

    // Menu sticky
    $(window).on('scroll',function() {    
      var scroll = $(window).scrollTop();
      if (scroll < 20) {
       $(".header-area").removeClass("sticky-header");
      }else{
       $(".header-area").addClass("sticky-header");
      }
   });
	
    //js code for mobile menu toggle
   $(".menu-toggle").on("click", function() {
       $(this).toggleClass("is-active");
   });

   // Banner Slider
   $('.hero').owlCarousel({
    loop:true,
    dots: true,
    autoplay: true,
    nav: false,
    mouseDrag: false,
    autoplayTimeout: 5000,
    smartSpeed: 1000,
    animateOut: 'fadeOut',
    animateIn: 'fadeIn',
    responsive:{
        0:{
            items:1,
            dots:false,
        },
        576:{
            items:1
        },
        991:{
            items:1
        },
        1024:{
            items:1
        },
        1199:{
            items:1
        }
    }
});

  // popular-items Slider
  $('.popular-items').owlCarousel({
  loop:true,
  dots: false,
  autoplay: true,
  nav: true,
  mouseDrag: false,
  autoplayTimeout: 5000,
  smartSpeed: 1000,
  animateOut: 'fadeOut',
  animateIn: 'fadeIn',
  margin: 30,
  navText: [
    '<i class="fa fa-angle-left"></i>',
    '<i class="fa fa-angle-right"></i>'
  ],
  responsive:{
    0:{
        items:1,
    },
    576:{
        items:2
    },
    991:{
        items:2
    },
    1024:{
        items:3
    },
    1199:{
        items:4
    }
  }
});

// screenshot popup
$(".screenshot-gallery").each(function () {
  $(this).find(".popup-screenshot").magnificPopup({
      type: "image",
      gallery: {
          enabled: true
      }
  });
}); 

// video popup
$('.video-popup').magnificPopup({
  type: 'iframe',
});





//portfolio filtering

var $portfolio = $('.portfolio');
if ($.fn.imagesLoaded && $portfolio.length > 0) {
    imagesLoaded($portfolio, function () {
        $portfolio.isotope({
            itemSelector: '.portfolio-item',
            filter: '*'
        });
        $(window).trigger("resize");
    });
}

$('.portfolio-filter').on('click', 'a', function (e) {
    e.preventDefault();
    $(this).parent().addClass('active').siblings().removeClass('active');
    var filterValue = $(this).attr('data-filter');
    $portfolio.isotope({filter: filterValue});
});


// Portfolio popup

$(".portfolio-gallery").each(function () {
    $(this).find(".popup-gallery").magnificPopup({
        type: "image",
        gallery: {
            enabled: true
        }
    });
}); 

$('.video-popup').magnificPopup({
    type: 'iframe',
});

//Counter-JS
$('.count').counterUp({
  delay: 10,
  time: 2000
});

// Photo Gallery
$('.photo-gallery').owlCarousel({
  loop: true,
  autoplay: true,
  nav: false,
  dots: false,
  autoplayTimeout:4000,
  navSpeed:800,
  autoplaySpeed:1000,
  margin: 30,
  responsive: true,
      responsive : {
          0 : {
              items: 1
          },
          480 : {
              items: 2,
          },
          768 : {
              items: 4,
          },
          1024 : {
              items: 6,
          }
      }
}); 



// Preloader Js
$(window).on('load', function(){
  $('.preloader').fadeOut(1000); // set duration in brackets    
});


    
// Increment and Decrement
$(document).ready(function() {
    const minus = $('.quantity__minus');
    const plus = $('.quantity__plus');
    const input = $('.quantity__input');
    minus.click(function(e) {
      e.preventDefault();
      var value = input.val();
      if (value > 1) {
        value--;
      }
      input.val(value);
    });
    
    plus.click(function(e) {
      e.preventDefault();
      var value = input.val();
      value++;
      input.val(value);
    })
  });
  

  // Price Range Slider

  $(function() {
   
    $( ".a" ).slider({
    range: true,
      min: 0,
    max: 500,
    values: [ 75, 300 ],
    slide: function( event, ui ) {
    $( ".b" ).text( "$" + ui.values[ 0 ] + " - $" + ui.values[ 1 ] );
    }
    });
    $( ".b" ).text( "$" + $( ".a" ).slider( "values", 0 ) +
    " - $" + $(".a" ).slider( "values", 1 ) );
       
    });
	
})(jQuery);