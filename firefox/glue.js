function onLoad() {
  browser.runtime.sendMessage({ "task": "init" }, function (response) {
    var app = Elm.Main.init({
      node: document.getElementById('elm'),
      flags: { windows: response.windows, visited: response.visited, prefs: response.prefs, colorOf: response.colorOf }
    });

    app.ports.createTab.subscribe(function (windowId) {
      browser.runtime.sendMessage({ "task": "createTab", "windowId": windowId });
      window.close();
    });

    app.ports.createWindow.subscribe(function () {
      browser.runtime.sendMessage({ "task": "createWindow" });
      window.close();
    });

    app.ports.removeTabs.subscribe(function (tabIds) {
      browser.runtime.sendMessage({ "task": "removeTabs", "tabIds": tabIds }, function (response) {
        app.ports.updatedTree.send(response.windows);
      });
    });

    app.ports.removeWindows.subscribe(function (windowIds) {
      browser.runtime.sendMessage({ "task": "removeWindows", "windowIds": windowIds });
      window.close();
    });

    app.ports.extractTabs.subscribe(function (tabIds) {
      browser.runtime.sendMessage({ "task": "extractTabs", "tabIds": tabIds });
      window.close();
    });

    app.ports.sortTabs.subscribe(function (tabIds) {
      browser.runtime.sendMessage({ "task": "sortTabs", "tabIds": tabIds }, function (response) {
        app.ports.updatedTree.send(response.windows);
      });
    });

    app.ports.pinTabs.subscribe(function (tabIds) {
      browser.runtime.sendMessage({ "task": "pinTabs", "tabIds": tabIds }, function (response) {
        app.ports.updatedTree.send(response.windows);
      });
    });

    app.ports.focusTab.subscribe(function (tabId) {
      browser.runtime.sendMessage({ "task": "focusTab", "tabId": tabId });
      window.close();
    });

    app.ports.openUrl.subscribe(function (url) {
      browser.runtime.sendMessage({ "task": "openUrl", "url": url });
      window.close();
    });

    app.ports.moveTabs.subscribe(function (arg) {
      var [tabIds, windowId, index] = arg;
      browser.runtime.sendMessage({ "task": "moveTabs", "tabIds": tabIds, "windowId": windowId, "index": index }, function (response) {
        app.ports.updatedTree.send(response.windows);
      });
    });

    app.ports.storeString.subscribe(function (arg) {
      var [name, string] = arg;
      browser.runtime.sendMessage({ "task": "storeString", "name": name, "string": string });
    });
  });
}

document.addEventListener("DOMContentLoaded", function () {
  onLoad();
});
