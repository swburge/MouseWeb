
// This recieves messages of type "testmessage" from the server.

//var gn ;
//Shiny.addCustomMessageHandler("testmessage",     
//    function(message) {
//     gn = message;
//    }
//);

Shiny.addCustomMessageHandler("testmessage",
  
  function drawCurrentGene(message) {
    b.search(message.first,function(err){
      if(err)
      console.log('Error in gene search: ' + err)
    }
    );
   var geneList = message.all;
   return geneList;
   //document.getElementById("genelist").innerHTML = message.all;
  
  }
 
);



function Variable(initVal, onChange)
    {
        this.val = initVal;
        this.onChange = onChange;

        this.GetValue = function()
            {
                return this.val;
            }
            
        this.SetValue = function(value)
            {
                this.val = value;
                this.onChange();
            }
    
    
      }
      
var myGene = new Variable(message)
 //function(message) {
//    alert("The b variable is " + message.b);
//  }