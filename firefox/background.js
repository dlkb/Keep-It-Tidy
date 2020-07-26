const EXTENSION_URL = "https://addons.mozilla.org/en-US/firefox/addon/kit/";
const MAX_LENGTH = 1024; // max length of the array keeping track of the browsing history
var visited = []; // tabIds of the last visited tabs, [last, next-to-last, ...]

browser.menus.create({
  title: "Rate Extension",
  contexts: ["browser_action"],
  onclick: function () {
    browser.tabs.create({ "url": EXTENSION_URL });
  }
});

function pushTo(arr, item) {
  arr.unshift(item);
  if (arr.length > MAX_LENGTH) {
    arr.pop();
  }
}

function getPrefs(callback) {
  browser.storage.sync.get(["alwaysOnPanel", "hints"], function (prefs) {
    callback(prefs);
  });
}

browser.runtime.onMessage.addListener(function (message, sender, sendResponse) {
  function getWindowsJson(callback) {
    browser.windows.getAll({ "populate": true, "windowTypes": ["normal"] }, function (windows) {
      callback(JSON.stringify(windows));
    });
  }
  function sendTree() {
    getWindowsJson(function (json) {
      sendResponse({ "windows": json });
    });
  }
  switch (message.task) {
    case "init":
      getPrefs(function (prefs) {
        getWindowsJson(function (json) {
          browser.storage.local.get(["colorOf"], function (result) {
            var colorOf = "";
            if (result.colorOf != undefined) {
              colorOf = result.colorOf;
            }
            sendResponse({ "windows": json, "visited": visited, "prefs": prefs, "colorOf": colorOf });
          });
        });
      });
      return true;
    case "createTab":
      browser.tabs.create({ "windowId": message.windowId });
      browser.windows.update(message.windowId, { "focused": true });
      return;
    case "createWindow":
      browser.windows.create();
      return;
    case "removeTabs":
      var tabIds = message.tabIds;
      browser.tabs.remove(tabIds, function () {
        if (browser.runtime.lastError) {
          console.warn("Whoops... " + browser.runtime.lastError.message);
        }
        sendTree();
        return;
      });
      return true;
    case "removeWindows":
      // in firefox closing a window also closes the popup, so we don't wait for the new tree
      var windowIds = message.windowIds;
      for (let windowId of windowIds) {
        browser.windows.remove(windowId, function () {
          if (browser.runtime.lastError) {
            console.warn("Whoops... " + browser.runtime.lastError.message);
            return;
          }
        });
      }
      return;
    case "extractTabs":
      var tabIds = message.tabIds;
      var firstTabId = tabIds.shift();
      // firefox doesn't support the focused property so the new window steals the focus automatically
      browser.windows.create({ "tabId": firstTabId }, function (win) {
        if (browser.runtime.lastError) {
          console.warn("Whoops... " + browser.runtime.lastError.message);
          return;
        }
        if (tabIds.length == 0) { // if only one tab in the selection
          return;
        }
        browser.tabs.move(tabIds, { "windowId": win.id, "index": -1 }, function () {
          if (browser.runtime.lastError) {
            console.warn("Whoops... " + browser.runtime.lastError.message);
          }
          return;
        });
      });
      return;
    case "pinTabs":
      var tabIds = message.tabIds;
      var count = 0;
      for (let tabId of tabIds) {
        browser.tabs.get(tabId, function (tab) {
          browser.tabs.update(tabId, { "pinned": !tab.pinned }, function () {
            if (browser.runtime.lastError) {
              console.warn("Whoops... " + browser.runtime.lastError.message);
              sendTree();
              return;
            }
            count++;
            if (count == tabIds.length) {
              sendTree();
              return;
            }
          });
        });
      }
      return true;
    case "sortTabs":
      var tabIds = message.tabIds;
      var tabs = [];
      for (let tabId of tabIds) {
        browser.tabs.get(tabId, function (tab) {
          tabs.push(tab);
          if (tabs.length == tabIds.length) {
            tabs.sort(function (tab1, tab2) {
              if (tab1.url < tab2.url) {
                return -1;
              }
              if (tab1.url > tab2.url) {
                return 1;
              }
            });
            var tabIdsOf = {};
            for (let tab of tabs) {
              if (tabIdsOf.hasOwnProperty(tab.windowId)) {
                tabIdsOf[tab.windowId].push(tab.id);
              } else {
                tabIdsOf[tab.windowId] = [tab.id];
              }
            }
            var count = 0;
            for (let [windowId, tabIds] of Object.entries(tabIdsOf)) {
              browser.tabs.move(tabIds, { "index": -1 }, function () {
                if (browser.runtime.lastError) {
                  console.warn("Whoops... " + browser.runtime.lastError.message);
                  sendTree();
                  return;
                }
                count++;
                if (count == Object.keys(tabIdsOf).length) {
                  sendTree();
                  return;
                }
              });
            }
          }
        });
      }
      return true;
    case "focusTab":
      browser.tabs.update(message.tabId, { "active": true }, function (tab) {
        browser.windows.update(tab.windowId, { "focused": true });
      });
      return;
    case "moveTabs":
      var tabIds = message.tabIds;
      var index = message.index;
      var windowId = message.windowId;
      browser.tabs.move(tabIds, { "index": index, "windowId": windowId }, function () {
        if (browser.runtime.lastError) {
          console.warn("Whoops... " + browser.runtime.lastError.message);
        }
        sendTree();
        return;
      });
      return true;
    case "openUrl":
      browser.tabs.create({ "url": message.url });
      return;
    case "storeString":
      var blob = {};
      blob[message.name] = message.string;
      browser.storage.local.set(blob);
      return;
  }
});

browser.tabs.onActivated.addListener(function (activeInfo) {
  pushTo(visited, activeInfo.tabId);
});

browser.windows.onFocusChanged.addListener(function (windowId) {
  if (windowId === browser.windows.WINDOW_ID_NONE) {
    return;
  }
  browser.tabs.query({ "active": true, "currentWindow": true, "windowType": "normal" }, function (tabs) {
    if (tabs.length > 0) {
      pushTo(visited, tabs[0].id);
    }
  });
});

var UPDATE_FROM_302 = "New look, the comeback of default selections, option to hide the browser panel, option to hide hints.";
var UPDATE_FROM_302_CONTEXT = "";

browser.runtime.onInstalled.addListener(function (details) {
  if (details.reason == "install") {
    browser.storage.sync.set({ "alwaysOnPanel": false, "hints": true });
  } else if (details.reason == "update") {
    if (isOlderThan(details.previousVersion, "3.1")) {
      browser.storage.sync.set({ "alwaysOnPanel": true, "hints": true });
      var options = {
        type: "basic",
        title: "Keep It Tidy",
        message: UPDATE_FROM_302,
        contextMessage: UPDATE_FROM_302_CONTEXT,
        iconUrl: "img/logo-128.png"
      }
      browser.notifications.create("update", options);
    }
  }
});

function isOlderThan(version, reference) {
  var versionParts = version.split(".");
  var referenceParts = reference.split(".");
  for (var i = 0; i < reference.length; i++) {
    var a = parseInt(versionParts[i]) || 0;
    var b = parseInt(referenceParts[i]) || 0;
    if (a < b) return true;
    if (a > b) return false;
  }
  return false;
}