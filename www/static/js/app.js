// handle NA values in leaflet raster map
$(document).on("DOMSubtreeModified", ".leaflet-control", function() {
  
  if ($(this).text().includes("NaN")) {
    
    $(this).text(
      $(this).text().replace(/NaN/g, "No Data")
    );
    
  }
  
});


// close download modal when download starts
$(document).on('shiny:download', function(event) {
    $('.modal').modal('hide');
});