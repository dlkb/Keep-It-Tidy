function onLoad() {
  chrome.runtime.sendMessage({ "task": "init" }, function (response) {
    var app = Elm.Main.init({
      node: document.getElementById('elm'),
      flags: { windows: response.windows, visited: response.visited }
    });

    app.ports.createTab.subscribe(function (windowId) {
      chrome.runtime.sendMessage({ "task": "createTab", "windowId": windowId });
      window.close();
    });

    app.ports.createWindow.subscribe(function () {
      chrome.runtime.sendMessage({ "task": "createWindow" });
      window.close();
    });

    app.ports.removeTabs.subscribe(function (tabIds) {
      chrome.runtime.sendMessage({ "task": "removeTabs", "tabIds": tabIds }, function (response) {
        app.ports.updatedTree.send(response.windows);
      });
    });

    app.ports.extractTabs.subscribe(function (tabIds) {
      chrome.runtime.sendMessage({ "task": "extractTabs", "tabIds": tabIds }, function (response) {
        app.ports.updatedTree.send(response.windows);
      });
    });

    app.ports.sortTabs.subscribe(function (tabIds) {
      chrome.runtime.sendMessage({ "task": "sortTabs", "tabIds": tabIds }, function (response) {
        app.ports.updatedTree.send(response.windows);
      });
    });

    app.ports.pinTabs.subscribe(function (tabIds) {
      chrome.runtime.sendMessage({ "task": "pinTabs", "tabIds": tabIds }, function (response) {
        app.ports.updatedTree.send(response.windows);
      });
    });

    app.ports.focusTab.subscribe(function (tabId) {
      chrome.runtime.sendMessage({ "task": "focusTab", "tabId": tabId });
      window.close();
    });

    app.ports.openUrl.subscribe(function (url) {
      chrome.runtime.sendMessage({ "task": "openUrl", "url": url });
      window.close();
    });

    app.ports.moveTabs.subscribe(function (arg) {
      var [tabIds, windowId, index] = arg;
      chrome.runtime.sendMessage({ "task": "moveTabs", "tabIds": tabIds, "windowId": windowId, "index": index }, function (response) {
        app.ports.updatedTree.send(response.windows);
      });
    });
  });
}

document.addEventListener("DOMContentLoaded", function () {
  onLoad();
});
