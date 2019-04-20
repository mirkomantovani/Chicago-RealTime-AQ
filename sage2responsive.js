var dimension = [0, 0];
$(document).on("shiny:connected", function (e) {

    //document.body.style.zoom = "100%";


    dimension[0] = window.innerWidth;
    dimension[1] = window.innerHeight;

    // switch_compare = document.getElementById("switch_compare");

    // document.getElementById("graphical_data_la").hidden = true;

    // switch_compare.onclick = function() {
    //     if(switch_compare.checked){
    //         // switch_compare.parentNode.getElementsByTagName('span')[0].innerText = "Show only one node data";
    //         document.getElementById("graphical_data_la").hidden = false;

    //     } else {
    //         // switch_compare.parentNode.getElementsByTagName('span')[0].innerText = "Switch to Daily Data (for 2018)";
    //         document.getElementById("graphical_data_la").hidden = true;
    //     }
    // };

    switch_units = document.getElementById("switch_units");

    // document.getElementById("yearly_inputs").hidden = true;

    switch_units.onclick = function() {
        if(switch_units.checked){
            switch_units.parentNode.getElementsByTagName('span')[0].innerText = "Switch to Metric units";
        } else {
            switch_units.parentNode.getElementsByTagName('span')[0].innerText = "Switch to Imperial units";
        }
    };

    // switch_top12_daily = document.getElementById("switch_top12_daily");

    // document.getElementById("dailyUniqueInputs").hidden = true;

    // switch_top12_daily.onclick = function() {
    //     if(switch_top12_daily.checked){
    //         document.getElementById("dailySeparateInputs").hidden = true;
    //         document.getElementById("dailyUniqueInputs").hidden = false;
    //     } else {
    //         document.getElementById("dailySeparateInputs").hidden = false;
    //         document.getElementById("dailyUniqueInputs").hidden = true;
    //     }
    // };

     nodes_table_switch.onclick = function() {
         if(nodes_table_switch.checked)
             document.getElementById("nodes").hidden = true;
         else
            document.getElementById("nodes").hidden = false;
     };

    //delete leaflet "ad" and move legend
    // leaflet = document.getElementsByClassName("leaflet-control");
    // leaflet[2].hidden = true;
    // leaflet[1].style.marginBottom = "100px";

    if (dimension[0] >= 2500) {  //SAGE
        //document.write("<style>.rule1 { ... }</style>");
        //document.body.style.fontSize = "500%";
        // console.log('porco dio cane');

        document.body.style.zoom = "250%";  //it was 400 so the 25% should become 40%

        // nozoom = document.getElementById("nozoom");
        // nozoom.style.zoom = "40%";

        // nozoom = document.getElementById("nozoom2");
        // nozoom.style.zoom = "40%";

        // document.getElementById("notforsage").hidden = true;


        // map = document.getElementById("map_county");
        //map.style.zoom = "200%"; //messes with the height and hover over markers
        // map.style.height = "1350px";

        // LEAFLET MAP TEXT size
        // leafletcont = document.getElementsByClassName("leaflet-container");
        //for (var i = 0; i < leafletcont.length; i++) {
        //leafletcont[i].style.fontSize = "60px";
        //}    not working because the javascript is executed first in this case


        boxzoom = document.getElementsByClassName("boxtozoom");
        for (var i = 0; i < boxzoom.length; i++) {
            boxzoom[i].style.zoom = "250%";
        }

        //WHOLE CONTENT PANEL SIZE
        cont = document.getElementsByClassName("content");
        cont[0].style.zoom = "40%";
        cont[0].style.fontSize = "100%";

        //BOX TITLES SIZE
        titles = document.getElementsByClassName("box-title");
        for (var i = 0; i < titles.length; i++) {
            titles[i].style.fontSize = "110%";
        }

        // FIRST SLIDER TEXT SIZE
        // slider = document.getElementsByClassName("irs-grid-text");
        // for (var i = 0; i < slider.length; i++) {
        //     if(slider[i].parentElement.parentElement.parentElement.parentElement.id == "nozoom")
        //     slider[i].style.fontSize = "200%";
        // }

        //change panels position in map
        // document.getElementById("counties_panel").style.right = "160px";
        // document.getElementById("counties_panel").style.top = "500px";
        // document.getElementById("counties_panel").style.bottom = "auto";
        document.getElementById("nodes").style.left = "450px";
        document.getElementById("nodes").style.top = "100px";




        // SECOND SLIDER
        // nozoomslider = document.getElementById("nozoomslider");
        // nozoomslider.style.zoom = "40%";
        
        //CONTROLS for map
        controls = document.getElementById("controls");
        controls.style.zoom = "140%";

        // darksky = document.getElementById("darksky");
        // darksky.style.zoom = "140%";

        nodes = document.getElementById("nodes");
        nodes.style.zoom = "140%";

        //map LEGEND ZOOM
        // document.getElementsByClassName("leaflet-control")[0].style.zoom = "300%";


        //DROPDOWN MENU NO ZOOM for problem in sage color picker
        //  drop = nozoomslider.getElementsByClassName("dropdown-shinyWidgets");
        // for (var i = 0; i < drop.length; i++) {
        //  drop[i].style.fontSize = "40%";
        // }
        //drop = document.getElementById("dropdown-menu-drop427388825");
        //drop.style.zoom = "40%";

        //Range slider no numbers
        // tags = nozoomslider.getElementsByClassName("irs-grid-text");
        // for (var i = 0; i < tags.length; i++) {
        //     tags[i].style.fontSize = "80%";
        // }
        // document.getElementsByClassName("irs-single")[0].style.fontSize = "200%";
        // document.getElementsByClassName("irs-min")[0].style.fontSize = "200%";
        // document.getElementsByClassName("irs-from")[1].style.fontSize = "200%";
        // document.getElementsByClassName("irs-from")[1].style.background = "#428bca00";
        // document.getElementsByClassName("irs-to")[1].style.fontSize = "200%";
        // document.getElementsByClassName("irs-to")[1].style.background = "#428bca00";

        // labels = document.getElementsByClassName("control-label");
        // labels[2].style.fontSize = "60px";


    }
    else {
        // map = document.getElementById("map_county");
        // map.style.height = "800px";

    }

    Shiny.onInputChange("dimension", dimension);
});
$(window).resize(function (e) {
    dimension[0] = window.innerWidth;
    dimension[1] = window.innerHeight;
    Shiny.onInputChange("dimension", dimension);
});
// $(function() {
//   $('#daily_aqi_line').click(function(){ $('#ggvis-tooltip').hide(); });
// });
// $(function() {
//   $('#daily_aqi_line_italy').click(function(){ $('#ggvis-tooltip').hide(); });
// });