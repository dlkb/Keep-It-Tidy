function onLoad() {
  browser.runtime.sendMessage({"task":"init"}, function(response){
    document.body.addEventListener("dragstart", function(event) { // Allows dragging in Firefox
      event.dataTransfer.setData("text/plain", null);
    });
    
    var app = Elm.Main.init({
      node: document.getElementById("elm"),
      flags: {windows: response.windows, visited: response.visited}
    });
    
    browser.runtime.onMessage.addListener(
      function(message, sender, sendResponse) {
        switch (message.task) {
        case "newTree":
          app.ports.updatedTree.send(message.windows);
          return;
        }
      }
    );
  
    app.ports.createTab.subscribe(function(windowId) {
      browser.runtime.sendMessage({"task":"createTab", "windowId":windowId});
    });
  
    app.ports.createWindow.subscribe(function() {
      browser.runtime.sendMessage({"task":"createWindow"});
    });
  
    app.ports.removeTabs.subscribe(function(tabIds) {
      browser.runtime.sendMessage({"task":"removeTabs", "tabIds":tabIds});
    });
  
    app.ports.extractTabs.subscribe(function(tabIds) {
      browser.runtime.sendMessage({"task":"extractTabs", "tabIds":tabIds});
    });
  
    app.ports.sortTabs.subscribe(function(tabIds) {
      browser.runtime.sendMessage({"task":"sortTabs", "tabIds":tabIds});
    });
  
    app.ports.pinTabs.subscribe(function(tabIds) {
      browser.runtime.sendMessage({"task":"pinTabs", "tabIds":tabIds});
    });
  
    app.ports.focusWindow.subscribe(function(windowId) {
      browser.runtime.sendMessage({"task":"focusWindow", "windowId":windowId});
      window.close();
    });
  
    app.ports.focusTab.subscribe(function(tabId) {
      browser.runtime.sendMessage({"task":"focusTab", "tabId":tabId});
      window.close();
    });
  
    app.ports.moveTabs.subscribe(function(arg) {
      var [tabIds, windowId, index] = arg;
      browser.runtime.sendMessage({"task":"moveTabs", "tabIds":tabIds, "windowId":windowId, "index":index});
    });

    app.ports.removeDuplicates.subscribe(function(tabIds) {
      browser.runtime.sendMessage({"task":"removeDuplicates", "tabIds":tabIds});
    });
  }); 
}

document.addEventListener("DOMContentLoaded", function() {
  onLoad();
});

window.addEventListener("unload", function() {
  browser.runtime.sendMessage({"task":"popupInactive"});
});
