
// This recieves messages of type "testmessage" from the server.

Shiny.addCustomMessageHandler("testmessage",
  
  function(message) {
    var para = document.createElement("P")
    var t = document.createTextNode(message)
    para.appendChild(t);
    document.body.appendChild(para);
    
  }
);

