function onLoad() {
  chrome.runtime.sendMessage({"task":"init"}, function(response){
    var app = Elm.Main.fullscreen({windows: response.windows, visited: response.visited});

    chrome.runtime.onMessage.addListener(
      function(message, sender, sendResponse) {
        switch (message.task) {
        case "newTree":
          app.ports.updatedTree.send(message.windows);
          return;
        }
      }
    );
  
    app.ports.createTab.subscribe(function(windowId) {
      chrome.runtime.sendMessage({"task":"createTab", "windowId":windowId});
    });
  
    app.ports.createWindow.subscribe(function() {
      chrome.runtime.sendMessage({"task":"createWindow"});
    });
  
    app.ports.removeTabs.subscribe(function(tabIds) {
      chrome.runtime.sendMessage({"task":"removeTabs", "tabIds":tabIds});
    });
  
    app.ports.extractTabs.subscribe(function(tabIds) {
      chrome.runtime.sendMessage({"task":"extractTabs", "tabIds":tabIds});
    });
  
    app.ports.sortTabs.subscribe(function(tabIds) {
      chrome.runtime.sendMessage({"task":"sortTabs", "tabIds":tabIds});
    });
  
    app.ports.pinTabs.subscribe(function(tabIds) {
      chrome.runtime.sendMessage({"task":"pinTabs", "tabIds":tabIds});
    });
  
    app.ports.focusWindow.subscribe(function(windowId) {
      chrome.runtime.sendMessage({"task":"focusWindow", "windowId":windowId});
    });
  
    app.ports.focusTab.subscribe(function(tabId) {
      chrome.runtime.sendMessage({"task":"focusTab", "tabId":tabId});
    });
  
    app.ports.moveTabs.subscribe(function(arg) {
      var [tabIds, windowId, index] = arg;
      chrome.runtime.sendMessage({"task":"moveTabs", "tabIds":tabIds, "windowId":windowId, "index":index});
    });
  }); 
}

document.addEventListener("DOMContentLoaded", function() {
  setTimeout(function() {
    onLoad();
  }, 1); // To prevent the "popup width and height aren't set" bug
});
