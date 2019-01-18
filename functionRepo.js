function extract_select2_data(node,leaves,index){
if (node.children){
    for(var i = 0;i<node.children.length;i++){
        index = extract_select2_data(node.children[i],leaves,index)[0];
    }
}
else {
    leaves.push({id:++index,text:node.presenterLast+", "+node.presenterFirst+" ("+node.name+")"});
}
return [index,leaves];
}

function hideToolTip(){
  div.style("opacity", 0);
    
  div.style("left", -1000 + "px")     
    .style("top", -1000 + "px");
}

function showTooltip(d,str){
  div.style("opacity", .9);
  div.html(str)
    .style("left", (d3.event.pageX + 28) + "px")     
    .style("top", (d3.event.pageY - 28) + "px");
}

function showAbstract(d){
  $("#dialog").html("Loading....");
  $("#dialog").dialog("option", "title", d.name).dialog("open");
  d3.json("json/" + d.name + ".json", function(error, root){
    //pubmed link for each author
    var author_arr = root.authors.split(", ");
    author_arr.forEach(function(x,index){
      author_arr[index] = "<a href='http://www.ncbi.nlm.nih.gov/pubmed/?term="+x.replace(' ', '+')+"' target='_blank'>"+ x +"</a>";
    })
    
    $("#dialog").html("<strong>Title: </strong>" + d.title + "<br/><strong>Authors:</strong> "+ author_arr.join(", ") + "<br/><br/><strong>Category: </strong>"+d.category+"<br/><strong>Keywords: </strong>"+root.keywords.replace(/;NA/gi,"")+"<br/><br/>" + root.text);
  });
}

  $('h2').on('click',function(){
    window.location.href = 'index.html';
  })