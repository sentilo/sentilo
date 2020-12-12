/**
 * Dashboard functions
 */

//config
var maxPointsToShowIntoChart = 30;

//order metrics
var orderMap = new Map();
orderMap.set("cpu",0);
orderMap.set("system_memory",1);
orderMap.set("process_memory",4);
orderMap.set("file_systems_0",3);
orderMap.set("file_systems_1",3);
orderMap.set("file_systems_2",3);
orderMap.set("threads",2);    
orderMap.set("events",5);
orderMap.set("data_sources",6);  
orderMap.set("thread_pool",7); 
orderMap.set("requests",1000);  

//color charts defined
var chartColors = {
  red: 'rgb(245, 80, 86)',
  orange: 'rgb(255, 159, 64)',
  yellow: 'rgb(255, 205, 86)',
  green: 'rgb(75, 192, 192)',
  blue: 'rgb(71, 171,241)',
  purple: 'rgb(136, 51, 170)',
  grey: 'rgb(251, 251, 251)'
};

//config metrics to convert
var unityConversor = ["file_systems","file_systems_0",
  "file_systems_1","system_memory","process_memory"]; 

//chart types per metric
var charts=["cpu", "system_memory", "threads"]
var chartNames=[];

//global variables
const diffMilisecons=120000;
var isReadableByte=false;
var progressBar;

var metrics=[];
var metricsOnRefresh=[];
var collectionCanvas=[];
var collectionCanvasOnRefresh=[];
var collectionChartTimes= new Map();

//load page
var main=function(data){
    
     
    collectionChartTimes=chartTimes(data);

    metrics=metricsName(data);
    
    $.each(data.artifactsMetrics, function(index, element ) {            
        var name = element.name.split(":");
        var tabname= name[1] + "-" + name[2];
        var state=getStatus(element.timestamp,element.state);
        buildAbstractElement(name[1],name[2], element.timestamp, state ) ;  
        buildTabContent(name[1],name[2]);
        buildCanvas(name[1],name[2],element.name,timeToDate(element.timestamp));        
        buildSingleObjects(element.metrics,tabname);        
    });

    collectionCanvas = collectionCanvasOnRefresh;
    activeTab();    
    buildConfigChartsByMetrics();
    getData(); 
    reorder();   
}


var bindData=function(data){    

    //chart times
    collectionChartTimes=chartTimes(data);

    //get new metrics
    metricsOnRefresh=metricsName(data);
    //clean abstract and metrics elements to refresh. Keep charts
    var activeMetric=$('#abstract').find('li.active').attr('id');
    cleanPage();       

    collectionCanvasOnRefresh=[];

    $.each(data.artifactsMetrics, function(index, element ) {                   
        var name = element.name.split(":");
        var tabname= name[1] + "-" + name[2];
        var state=getStatus(element.timestamp,element.state);
        buildAbstractElement(name[1],name[2], element.timestamp, state ) ;  
        buildTabContent(name[1],name[2]); 
        buildCanvas(name[1],name[2],element.name,timeToDate(element.timestamp));
                
        buildSingleObjects(element.metrics,tabname);        
    });

    collectionCanvas = collectionCanvasOnRefresh;
    
    buildConfigChartsByMetrics();
    getData();
    activeTabAfterRemove(activeMetric);
    reorder();           
}

var chartTimes=function(data){
    var d = new Map()
    $.each(data.artifactsMetrics, function(index, element ) {  
        var artefact = element.name.split(":");
        var name= artefact[1] + "-" + artefact[2];         
        d.set(name,element.timestamp);
    });
    return d;
}

var metricsName=function(data){
    var metrics=[];
    $.each(data.artifactsMetrics, function(index, element ) {   
        var name = element.name.split(":");        
        var tabname= name[1] + "-" + name[2];
        metrics.push(tabname);
    });
    return metrics;
}

var buildSingleObjects=function(obj, tabname, k){    
    Object.entries(obj).forEach(entry => {
        let key = entry[0];
        let value = entry[1];          
        if(typeof value == 'object'){  
            if(key=='requests' && value.length == undefined){ 
                
                $(jqEscape(tabname+"content")+" ul").last().append(
                    buildAgroupper(tabname,key,key));

                buildSimpleObjects(value,tabname, key, false);
            
            }else if (key=='requests' && value.length > 0){
                Object.entries(value).forEach(entry => {
                    let k = entry[0];
                    let v = entry[1];
                    $(jqEscape(tabname+"content")+" ul").last().append(
                        buildAgroupper(tabname,key+"_"+k,key));

                    buildSimpleObjects(v,tabname, key+"_"+k,false);                
                });

            }else if(key=='process_memory'){
                
                $(jqEscape(tabname+"content")+" ul").last().append(
                    buildAgroupper(tabname,key,key));

               buildSimpleObjects(value,tabname, key, true);

            }else if(value.length>0){
                Object.entries(value).forEach(entry => {
                    let k = entry[0];
                    let v = entry[1];
                    $(jqEscape(tabname+"content")+" ul").last().append(
                        buildAgroupper(tabname,key +"_"+k,key));

                   
                    $("[id^='"+ tabname +"'][id$='Stats']").last().append(
                        buildProgressBar(v));
                    

                    buildSimpleObjects(v,tabname, key+"_"+k, false);                
                });                     
            }else{
            
                $(jqEscape(tabname+"content")+" ul").last().append(
                    buildAgroupper(tabname,key,key));

               
               $(jqEscape(tabname+"_"+key+"Stats")).last().append(
                    buildProgressBar(value));
               

                isReadableByte=isReadableByteFunction(key);  
                
                 
                buildSingleObjects(value,tabname, key)  
            }               
          
        }else{         
        
            $(jqEscape(tabname+"_"+k +"Stats")).last().append(
                 buildElement(tabname,value,key,'',isReadableByte));  
        }
    });
}

var buildSimpleObjects=function(obj, tabname, key, showProgressBar){

    Object.entries(obj).forEach(entry => {
        let k = entry[0];
        let v = entry[1]; 
        if(typeof v == 'object'){ 
            $(jqEscape(tabname+"_"+key+"Stats")).last().append(
                buildSubAgroupper(tabname,key,k));               
            
            if(showProgressBar){
                $("[id^='"+ tabname +"'][id$='Stats']").last().append(
                        buildProgressBar(v));
            
            }

            buildSimpleObjects(v,tabname,key);

        }else{

            isReadableByte=isReadableByteFunction(key);             
             $("[id^='"+ tabname +"'][id$='Stats']").last(".agrouper").append(
                 buildElement(tabname,v,k,'',isReadableByte));  
        } 
    });
}

var cleanPage=function(){    
    $("#abstract ul li").remove();
    $(".tabcontent [id$='_element'] li").remove();   
    for(var m in metrics){ 
        if(!metricsOnRefresh.includes(metrics[m])){
            $(jqEscape(metrics[m]+"content")).remove(); 
        }
    }

    metrics=metricsOnRefresh;
    metricsOnRefresh=[];
}

var activeTabAfterRemove=function(activeMetric){    
	if($(jqEscape(activeMetric)).length==0){	
        activeTab();
    }else{
        $(jqEscape(activeMetric)).addClass('active');
        $('#metrics-name small').text($('#abstract li.active').attr('id'));    
        $('.tabcontent').hide();
        $(jqEscape(activeMetric+'content')).show();   
    }
}

var buildAbstractElement= function (host, artefact , lastTime, status){
    var element =  '<li id="'+host+"-"+artefact+'">';
    element +='<div class="accordion" id="activityAccordion'+host+"-"+artefact+'">';
    element +='<div class="accordion-group">';
    element +='<div class="accordion-heading">';
    element +='<a class="accordion-toggle" data-toggle="collapse" data-parent="#activityAccordion'+host+"-"+artefact+'"';
    element +='href="#activityAccordionCollapse'+host+"-"+artefact+'"> <i class="icon-signal"></i> '+artefact;
    element +='<i class="icon-chevron-down pull-right"></i> </a>';
    element +='</div>';
    element +='<div id="activityAccordionCollapse'+artefact+'" class="accordion-body collapse in">';
    element +='<div class="accordion-inner metrics">';
    element +='<span>'+host+'</span><br><span>'+getStatusString(status)+'</span><br><span class="infobox-footer">'+ timeToDate(lastTime) +'</span><br>';
    element +='</div>';    
    element +='</div>';
    element +='</div>';
    element +='</div>';
    element +='</div></li>';
    $("#abstract ul").append(element);
    
}

var buildCanvas=function(host, artefact, metricname,time){
    var name = host +"-"+ artefact;  
    var element="";
    for(var chart in charts){  
         if($(jqEscape(name+"_chart_"+charts[chart])).length==0){ 
        	element +='<div class="accordion accustom" id="activityAccordion'+name+"_chart_"+charts[chart]+'">';
            element +='<div class="accordion-group">';
            element +='<div class="accordion-heading">';
            element +='<a class="accordion-toggle" data-toggle="collapse" data-parent="#activityAccordion'+name+"_chart_"+charts[chart]+'"';
            element +='href="#activityAccordionCollapse'+name+"_chart_"+charts[chart]+'"> <i class="icon-signal"></i> '+charts[chart];
            element +='<i class="icon-chevron-down pull-right"></i> </a>';
            element +='</div>';
            element +='<div id="activityAccordionCollapse'+name+"_chart_"+charts[chart]+'" class="accordion-body collapse in">';
            element +='<div class="accordion-inner metrics">';
            element +='<div id="placeholder" class="ct-chart-centered-labels">';
            element +='<div id="'+name+"_chart_"+charts[chart]+'" data-parent="'+ name + "_" +charts[chart]+'" class="chart" style="height:216px;margin-right:25px;background-color: #fbfbfb;margin-bottom:50px;margin-top:25px"></div>';
            element +='</div>';
            element +='</div>';
            element +='</div>';
            element +='</div>';
            element +='</div>';

            if(!collectionCanvasOnRefresh.includes(name+"_chart_"+charts[chart]) && charts[chart] != undefined ){
                collectionCanvasOnRefresh.push(name+"_chart_"+charts[chart]);                            
            }
         }         
    }
    $(jqEscape(name+"_chart")).append(element);
}

var refreshChartsTimes=function(host, artefact, metricname,time){
    var name = host +"-"+ artefact;
    collectionChartTimes.set(name, time);
}

var buildTabContent=function(host, artefact){
    var name = host +"-"+ artefact;  
    var element=""; 
    if(($(jqEscape(name+"content")).length==0)){
            element +="<div id='"+name+"content' class='tabcontent'>";
            element +="<div id='"+name+"_chart'></div>";
            element +="<ul id='"+name+"_element'></ul></div>"; 
            element +="</div>";         
        $("#container").append(element);
    }
}

var activeTab=function(){
    if($('#abstract li').hasClass('active')){
         return;   
    }else{
        $('#abstract li:first').addClass('active'); 
        $('#metrics-name small').text($('#abstract li.active').attr('id'));
        $('.tabcontent').hide();
        $('.tabcontent:first').show();
    }    
}

var buildAgroupper=function(tabname,key,label){    
    var id=tabname+"_"+key;
    var element=''; 
    element+="<li  data-node='" + order(key)+ "'>"
    element+="<div class=''>";
    element+="<div class='accordion' id='"+id+"'>";
    element+="<div class='accordion-group'>";
    element+="<div class='accordion-heading'>";
    element+="<span>";
    element+="<a class='accordion-toggle' data-toggle='collapse' data-parent='#"+id+"Accordion' href='#"+id+"AccordionCollapse'> <i class='icon-map-marker'></i>"+label;
    element+="<i class='icon-chevron-down pull-right'></i> </a>";    
    element+="<div id='"+id+"AccordionCollapse' class='accordion-body in collapse' style='height: auto;'>";
    element+="<div class='accordion-inner metrics' id='"+id+"Stats'>";    
    element+="</div>";       
    element+="</div>";
    element+="</div>";
    element+="</div>";    
    element+="</div>"; 
    element+="</li>";   
    return element; 
}

var buildSubAgroupper=function(tabname,parentDiv,property){      
    var id=tabname+"-"+parentDiv+"_"+property;
    var element='';
    element+="<div id='"+id+"Stats' class=''><div class='agrouper'><strong>"+property+"</strong></div>";  
    element+="<div id='"+id+"progressBar' class=''><div class='agrouper'></div>";    
    return element;
}

var buildElement=function(tabname,obj, property,parent,isReadableByte){    
    var element='';
    element+="<div id='"+property+"_stats' class=''>";
    if(isReadableByte){
        element +="<span class='key'>"+property+"</span><span class='value'>"+readableBytes(obj)+"</span>";
    }else{
        element +="<span class='key'>"+property+"</span><span class='value'>"+obj+"</span>";
    }      
    element+="</div>"; 
    
    return element;
}

var buildProgressBar=function(obj){    
    var element='';
    var p=percent(obj);           
                                 
    if(p>0){        
        element += '<div class="progress">'; 
        element += '<div class="bar ' + getStyleBar(p) + '" style="width:'+p+'%" aria-valuenow="25" aria-valuemin="0" aria-valuemax="100">'+p+'%</div>';
        element +='</div>'        
    }else{
        element += '<div class="progress emptyprogress"></div>' ; 
    }           
    return element;
}

var getStatus=function(t, s){
    var diff = compareDates(t);
    if(diff>diffMilisecons){         
        return "RED";            
    }else{
        return s;
    } 
}

var getStatusString=function(status){    
    if(status=="GREEN"){
        return '<span class="indicator label-success"></span>running';
    }
    else if(status=="ORANGE"){
        return '<span class="indicator label-warning"></span>running';    
    }else{
        return '<span class="indicator label-danger"></span>stopped';
    }
}

var percent=function(obj){     
    var res;
    if(obj <= 0) return obj;    
    if(obj.hasOwnProperty('free') && obj.hasOwnProperty('total')){
        if(obj.free==0)return 0;
        res= parseFloat(parseInt(obj.free, 10) * 100)/ parseInt(obj.total, 10);
        if(res<=0)return 0;
        return (Math.round(100 - res))
    }
    if(obj.hasOwnProperty('used') && obj.hasOwnProperty('max')){
        if(obj.used==0)return 0;
        res= parseFloat(parseInt(obj.used, 10) * 100)/ parseInt(obj.max, 10); 
        if(res<=0)return 0;
        return (Math.round(res))
    }    
}

var getStyleBar=function (obj){
    if(obj < 61){
        return "bar-success";
    }else if(obj < 90){
        return "bar-warning";
    }else if (obj >= 90){
        return "bar-danger";
    }
}

var isReadableByteFunction=function(property){  
    if(unityConversor.includes(property)){
        return true;
    }
    return false;
}

var readableBytes=function (bytes) {
    if(Number.isInteger(bytes) && bytes>1024){
        var i = Math.floor(Math.log(bytes) / Math.log(1024)),
        sizes = ['B', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB'];
        return (bytes / Math.pow(1024, i)).toFixed(2) * 1 + ' ' + sizes[i];
    }else{
        return bytes;
    }
}

var compareDates=function(date){ 
   var now= $.now();
   var res = now - date;   
   return res;
}

var timeToDate=function(time){
    var date = new Date(time);
    var formatDate = date.getFullYear() + '-' +('0' + (date.getMonth()+1)).slice(-2)+ '-' +  date.getDate() +
         ' '+date.getHours()+ ':'+('0' + (date.getMinutes())).slice(-2)+ ':'+date.getSeconds();
    return formatDate.toString();
}

var escapeRegExp=function (str) {
    return str.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
}

var order=function(key){  
    var order=1000;
    if(orderMap.get(key) != undefined){
        order=orderMap.get(key)
    }
    
    return order;
}

var reorder=function(){
    var id = $('#abstract').find('li.active').attr('id');   
    var idContent="#"+id+"_element";
    $(idContent).find('li').sort(function (a, b) {
        return +a.dataset.node - +b.dataset.node;
    }).appendTo( $(idContent) );
}

var buildConfigChartsByMetrics=function(){
    for(var c in collectionCanvas){       
         if(window.collectionCanvas[c]!=undefined){              
            configChart(collectionCanvas[c]);     
         } 
    }
}

var configChart=function(metric){
    

    if(metric.includes('cpu') ){
        window.metric= chartMetrics(metric,series(['system_load','process_load']),
            ['system_load','process_load']);            
                
    }else if(metric.includes('system_memory')) {
        window.metric=chartMetrics(metric,series(['used','free']),
            ['used','free']);          

    }else if(metric.includes('threads')){
        window.metric=chartMetrics(metric, series(['total','daemon']),
            ['total','daemon']);
    }else{
        return;
    }
    
    chartNames.push(window.metric);
}

var series=function(labels){
    var obj=[];
    for (i = 0; i < labels.length; i++) {
        var id = labels[i];        
        tmp = {
            name: id,
            data: []
        };
        obj.push(tmp);
    }

    return JSON.stringify(obj);
}

var getData = function() {
    
	for (var c in chartNames){ 
    var newTime=addTimeValue(chartNames[c]);
    if(newTime != window.chartNames[c].data.labels[window.chartNames[c].data.labels.length -1]){
        // Limit data to display to the last 30 points (more points transform chart into something unusable)
        if(window.chartNames[c].data.labels.length > maxPointsToShowIntoChart){
        	window.chartNames[c].data.labels.shift();
        	window.chartNames[c].data.series.forEach(function(serie) {            
                serie.data.shift();
            });        	
        }
        
        window.chartNames[c].data.labels.push(newTime);    
        window.chartNames[c].data.series.forEach(function(serie) {            
            serie.data.push({x:convertTimeToLong(newTime), y:addGlobalValue(chartNames[c],  serie)});            
        });        
        window.chartNames[c].update(window.chartNames[c].data);
    }
  }
}

var addGlobalValue=function(chart,serie) { 
    var parent =chart.container.dataset.parent; 
    var label = serie.name;
    var value = $(jqEscape(parent)+" "+jqEscape(label+'_stats')+" span.value").text().split(" ");   
    return value[0] ;
}

var addTimeValue = function(chart) { 
    var parent =chart.container.dataset.parent;   
    var artefact=parent.split("_");    
    var value =  new Date(collectionChartTimes.get(artefact[0]));    
    var h = addZero(value.getHours());
    var m = addZero(value.getMinutes());
    var s = addZero(value.getSeconds());
    return  h + ":" + m + ":" + s;
}

var addZero = function(i) {
  if (i < 10) {
    i = "0" + i;
  }
  return i;
}

function jqEscape(elementId) {	 
  // Due to elementId could contains dots (e.g. dev.sentilo.io-mock-agent), jQuery selectors need to escape meta-characters
  // https://api.jquery.com/category/selectors/
  // https://learn.jquery.com/using-jquery-core/faq/how-do-i-select-an-element-by-an-id-that-has-characters-used-in-css-notation/	
  return "#" + elementId.replace( /(:|\.|\[|\]|,|=|@)/g, "\\$1" ); 
}

