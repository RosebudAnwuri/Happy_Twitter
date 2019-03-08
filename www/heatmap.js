$(document).on({
	'shiny:connected': function(event){
	console.log('Tried!');
	$('.icon i.fas.fa-fab').removeClass('fas').removeClass('fa-fab').removeClass('fa-twitter').addClass('fab fa-twitter');
	
	},
	'shiny:value': function(event){
	  var boxPlot_width=$('#default-gender_bplot svg').attr('data-width');
	var boxPlot_height=$('#default-gender_bplot svg').attr('data-height');
	$('#default-gender_bplot svg').attr('width',boxPlot_width);
	$('#default-gender_bplot svg').attr('height',boxPlot_height);
	$('#default-gender_bplot').css('width',boxPlot_width);
	$('#default-gender_bplot').css('height',boxPlot_height);
	
	}
	

  }); 
  
  $(document).ready(function(){
	  $('#tab-main').on('click', function() {
  
    $('#tab-about').removeClass('active show');
	
	 $('#tab-main').addClass('active show');
	
  });  



$('#tab-about').on('click', function() {
    
  $('#tab-about').addClass('active show');
	
	 $('#tab-main').removeClass('active show');
	 
  }); 
  })
  
 
	