document.addEventListener("DOMContentLoaded", function(event){
	// Get document's head
	var head = document.head;

	function setAttributes(el, attrs) {
	  for(var key in attrs) {
		el.setAttribute(key, attrs[key]);
	  }
	};

	// Creating a style tag for sliders
	var style = document.createElement("style");
	style.type = "text/css";

	var css = ".slider-color { -webkit-appearance: none; width: 100%; height: 15px; border-radius: 5px; background: #d3d3d3; outline: none; opacity: 0.7; -webkit-transition: opacity .15s ease-in-out; transition: opacity .15s ease-in-out;} .setText { position: absolute; top: 3.5em; text-align: center; color: red; font-weight: bold; width: 60px; height: 1em; white-space: nowrap;}";

	if (style.styleSheet){
	  style.styleSheet.cssText = css;
	} else {
	  style.appendChild(document.createTextNode(css));
	}
	head.appendChild(style);

	// Functions
	function onClickLegendSelect(event){
		var targetElement = event.target || event.srcElement;
		var toPruneList = [];
		
		if(selectedRects.length >= 1){
			for(var i = 0; i < selectedRects.length ; i++){
				//console.log(targetElement.getAttribute("title") === selectedRects[i].getAttribute("title"));
				if(targetElement.getAttribute("title") === selectedRects[i].getAttribute("title")){
					toPruneList.push(selectedRects[i]);
				}
			}
			if(toPruneList.length < 1){
				selectedRects.push(targetElement);
			}
		} else {
			selectedRects.push(targetElement);
		}
		console.log(toPruneList);
		for(var i = 0; i < toPruneList.length; i++){
			selectedRects.splice(selectedRects.indexOf(toPruneList[i]), 1);
		}
		updateSettingsChanged();
	}
	function hasValueInDict(el, val){
		return Object.keys(el).find(k => el[k] === val)
	}

	function updateSettingsChanged(){
		// Reset all elements
		legendRects.map(k => legendRects[k].setAttribute("opacity", "100%"));
		dataList.map(k => k["svgId"].setAttribute("opacity", "100%"))
		// Modify legends selection first
		var prunedSelection = [];
		selectedRects.map(k => k.setAttribute("opacity", "10%"));
			
		for(var i = 0; i < dataList.length; i++){
			var shouldPush = true;
			for(var j = 0; j < selectedRects.length; j++){
				if(hasValueInDict(dataList[i], selectedRects[j].getAttribute("title"))){
					shouldPush = false;
				}
			}
			if(shouldPush){
				prunedSelection.push(dataList[i]);
			} else {
				dataList[i].svgId.setAttribute("opacity", "5%");
			}
		}
		var selectedData = prunedSelection.length > 0 ? prunedSelection : dataList;
		
		for(var i = 0; i < selectedData.length; i++){
			for(var j = 0; j < dataCurrentRange.length; j++){
				if(parseFloat(selectedData[i][dataCurrentRange[j][0]]) < parseFloat(dataCurrentRange[j][1]) || parseFloat(selectedData[i][dataCurrentRange[j][0]]) > parseFloat(dataCurrentRange[j][2])){
					selectedData[i].svgId.setAttribute("opacity", "5%");
					break;
				} else if (j == dataCurrentRange.length - 1) {
					selectedData[i].svgId.setAttribute("opacity", "100%");
				}
			}
		}
	}

	function createSliderNumeric(titleText, min, max){
		var sliderDiv = document.createElement("div");
		setAttributes(sliderDiv, {"class": "slider", "style": "position: relative", "min": min, "max": max, "id": titleText});
		
		var slider = document.createElement("div");
		slider.setAttribute("id", "slider-range-" + titleText);
		slider.setAttribute("class", titleText);
		
		var textSlider = document.createElement("h4");
		textSlider.innerText = "Display " + titleText + " over:";

		var titleSlider = document.createElement("h5");
		titleSlider.setAttribute("class", titleText);
		titleSlider.innerHTML = min + " - " + max;

		sliderDiv.append(textSlider);
		sliderDiv.append(slider);
		sliderDiv.append(titleSlider);
		
		return sliderDiv;
	}




	// dataList: Array of all bubbles html elements
	var dataList = [];

	// dataDictKeys: Array of all exported keys/tooltips available when hovering over a bubble
	var dataDictKeys = [];

	// dataKeysNumeric: Subset array of dataDictKeys where keys are represented by numerical values
	var dataKeysNumeric = [];

	// dataNumericsRange: The range of values of each keys in dataKeysNumeric
	var dataNumericsRange = [];

	// dataCurrentRange: The range of the current user set value based on the slider's position
	var dataCurrentRange = [];

	// rePolyTitle: Extracting exported information from generated tooltip from R
	var rePolyTitle = /([\s\S]*): ([\s\S]*)/;

	var svgPolys = document.getElementsByTagName("polygon");
	// Get all attributes 
	if(svgPolys.length > 0){
		var i = 0;
		var foundKeys = false;
		while(i < svgPolys.length && !foundKeys){
			if(svgPolys[i].hasAttribute("title")){
				var listOfData = svgPolys[i].getAttribute("title").split("&lt;br/&gt;");
				var numKeys = listOfData.length - 1;
				for(var j = 0; j < numKeys; j++){
					var regexResult = rePolyTitle.exec(listOfData[j]);
					dataDictKeys.push(regexResult[1]);
					if(!isNaN(parseFloat(regexResult[2]))){
						dataKeysNumeric.push(regexResult[1]);
						dataNumericsRange.push([regexResult[1], NaN, NaN]);
					}
				}
				foundKeys = true;
			}
			else{
				i++;
			}
		}
		
	}

	for(var i = 0; i < svgPolys.length; i++){
		if(svgPolys[i].hasAttribute("title")){
			// Temporarily store the polygon for future manipulation
			var svgId = svgPolys[i]
			var listOfData = svgPolys[i].getAttribute("title").split("&lt;br/&gt;");
			
			var tmpDict = {};
			for(var j = 0; j < listOfData.length-1; j++){
				var regexResult = rePolyTitle.exec(listOfData[j]);
				tmpDict[regexResult[1]] = regexResult[2];
				if(dataKeysNumeric.includes(regexResult[1])){
					for(var k = 0; k < dataKeysNumeric.length; k++){
						if(dataNumericsRange[k][0] == regexResult[1]){
							if(parseFloat(regexResult[2]) < parseFloat(dataNumericsRange[k][1]) || isNaN(dataNumericsRange[k][1])){
								dataNumericsRange[k][1] = regexResult[2];
							}
							if(parseFloat(regexResult[2]) > parseFloat(dataNumericsRange[k][2]) || isNaN(dataNumericsRange[k][2])){
								dataNumericsRange[k][2] = regexResult[2];
							}
							
						}
					}
				}
			}
			tmpDict["svgId"] = svgId;
			dataList.push(tmpDict);
		}
	}

	dataCurrentRange = dataNumericsRange.slice();

	var parametersDiv = document.createElement("div");
	parametersDiv.setAttribute("style", "top: 45px; right: 20px; position: absolute");
	//setAttributes(parametersDiv, {"top": "3px", "right": "20px", "position": "absolute"});


	for(var i = 0; i < dataNumericsRange.length; i++){
		var selectedNumericKey = dataNumericsRange[i]
		var slider = createSliderNumeric(selectedNumericKey[0], selectedNumericKey[1], selectedNumericKey[2])
		parametersDiv.append(slider);
	}

	// Get container
	var gContainer = document.getElementsByClassName("girafe_container_std")[0];
	gContainer.append(parametersDiv);


	// Finding legend elements

	// selectedRects contains legend elements with hover tooltip indicating their properties
	var selectedRects = [];
	var legendRects;

	var waitForJQuery = setInterval(function () {
		if (typeof $ != "undefined") {
			// https://jqueryui.com/slider/#range
			legendRects = $("rect[title]");
			for(var i = 0; i < legendRects.length; i++){
				legendRects[i].addEventListener("click", onClickLegendSelect);
			}
			$(".slider").each(function(){
				var $this = $(this);
				var tmin = parseFloat($(this).attr("min")),
						tmax = parseFloat($(this).attr("max"));
				var qKey = $(this).attr("id");
				$("div[id^=slider-range]", $this).slider({
					range: true,
					min: 0,
					max: 100,
					values: [0, 100],
					slide: function(event, ui){
						var steps = (tmax - tmin)/100;
						var r1 = steps * parseFloat(ui.values[0]) + tmin;
						var r2 = steps * parseFloat(ui.values[1]) + tmin;
						$("h5", $this).text(r1.toFixed(8) + " - " + r2.toFixed(8));
						for(var i = 0; i < dataCurrentRange.length; i++){
							if(dataCurrentRange[i][0] == qKey){
								dataCurrentRange[i][1] = r1;
								dataCurrentRange[i][2] = r2;
								break;
							}
						}
						//displayNumericDatas(dataList, qKey, r1, r2);
						updateSettingsChanged();
					}
				})
			});
			
			clearInterval(waitForJQuery);
		}
	}, 10);
})

