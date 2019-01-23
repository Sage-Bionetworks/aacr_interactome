function extract_select2_data(node,leaves,index){
  if (node.children){
      for(var i = 0;i<node.children.length;i++){
          index = extract_select2_data(node.children[i],leaves,index)[0];
      }
  }
  else {
      var temp = {id:node.id,text:node.presenterLast+", "+node.presenterFirst+" ("+node.id+")"};
      var found = leaves.some(function (el) {
        return el.id === node.id;
      });
      if (!found) { leaves.push(temp); }
  }
  return [index,leaves];
}

function processSelect2Data(data){
  select2_data = extract_select2_data(data,[],0)[1];
  select2_data.sort(function(a, b) {
    var string1 = a.text;
    var string2 = b.text;
    return (string1 > string2) - (string1 < string2);
  });
  return select2_data;
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
  $("#dialog").dialog("option", "title", d.id).dialog("open");
  d3.json("json/" + d.id + ".json", function(error, root){
    //pubmed link for each author
    var author_arr = root.authors.split(", ");
    if(root.authors == ""){
      var temp = d.presenter_first + " " + d.presenter_last; 
      author_arr.push(temp);
    }
    author_arr.forEach(function(x,index){
      author_arr[index] = "<a href='http://www.ncbi.nlm.nih.gov/pubmed/?term="+x.replace(' ', '+')+"' target='_blank'>"+ x +"</a>";
    })
    
    $("#dialog").html("<strong>Title: </strong>" + d.title + "<br/><strong>Authors:</strong> "+ author_arr.join(", ") +"<br/><strong>Keywords: </strong>"+root.keywords+"<br/><br/>" + root.text);
  });
}

  $('h2').on('click',function(){
    window.location.href = 'index.html';
  })