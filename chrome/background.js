const MAX_LENGTH = 1024; // max length of the array keeping track of the browsing history
var visited = []; // tabIds of the last visited tabs, [last, next-to-last, ...]

function pushTo(arr, item) {
  arr.unshift(item);
  if (arr.length > MAX_LENGTH) {
    arr.pop();
  }
}

function getPrefs(callback) {
  chrome.storage.sync.get(["alwaysOnPanel", "hints"], function (prefs) {
    callback(prefs);
  });
}

chrome.runtime.onMessage.addListener(function (message, sender, sendResponse) {
  function getWindowsJson(callback) {
    chrome.windows.getAll({ "populate": true, "windowTypes": ["normal"] }, function (windows) {
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
          chrome.storage.local.get(["colorOf"], function (result) {
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
      chrome.tabs.create({ "windowId": message.windowId });
      chrome.windows.update(message.windowId, { "focused": true });
      return;
    case "createWindow":
      chrome.windows.create();
      return;
    case "removeTabs":
      // it's a workaround because there is a bug in chrome, the callback from tabs.remove executes before every tab has been closed
      var tabIds = message.tabIds;
      var count = 0;
      function onTabRemoved() {
        count++;
        if (count == tabIds.length) {
          chrome.tabs.onRemoved.removeListener(onTabRemoved);
          sendTree();
          return;
        }
      }
      chrome.tabs.onRemoved.addListener(onTabRemoved);
      chrome.tabs.remove(tabIds, function () {
        if (chrome.runtime.lastError) {
          console.warn("Whoops... " + chrome.runtime.lastError.message);
          chrome.tabs.onRemoved.removeListener(onTabRemoved);
          sendTree();
          return;
        }
      });
      return true;
    case "removeWindows":
      // same workaround because same type of callback as tabs.remove
      var windowIds = message.windowIds;
      var count = 0;
      function onWindowRemoved() {
        count++;
        if (count == windowIds.length) {
          chrome.windows.onRemoved.removeListener(onWindowRemoved);
          sendTree();
          return;
        }
      }
      chrome.windows.onRemoved.addListener(onWindowRemoved);
      for (let windowId of windowIds) {
        chrome.windows.remove(windowId, function () {
          if (chrome.runtime.lastError) {
            console.warn("Whoops... " + chrome.runtime.lastError.message);
            chrome.windows.onRemoved.removeListener(onWindowRemoved);
            sendTree();
            return;
          }
        });
      }
      return true;
    case "extractTabs":
      var tabIds = message.tabIds;
      var firstTabId = tabIds.shift();
      chrome.windows.create({ "focused": false, "tabId": firstTabId }, function (win) {
        if (chrome.runtime.lastError) {
          console.warn("Whoops... " + chrome.runtime.lastError.message);
          sendTree();
          return;
        }
        if (tabIds.length == 0) { // if only one tab in the selection
          sendTree();
          return;
        }
        chrome.tabs.move(tabIds, { "windowId": win.id, "index": -1 }, function () {
          if (chrome.runtime.lastError) {
            console.warn("Whoops... " + chrome.runtime.lastError.message);
          }
          sendTree();
          return;
        });
      });
      return true;
    case "pinTabs":
      var tabIds = message.tabIds;
      var count = 0;
      for (let tabId of tabIds) {
        chrome.tabs.get(tabId, function (tab) {
          chrome.tabs.update(tabId, { "pinned": !tab.pinned }, function () {
            if (chrome.runtime.lastError) {
              console.warn("Whoops... " + chrome.runtime.lastError.message);
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
        chrome.tabs.get(tabId, function (tab) {
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
              chrome.tabs.move(tabIds, { "index": -1 }, function () {
                if (chrome.runtime.lastError) {
                  console.warn("Whoops... " + chrome.runtime.lastError.message);
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
      chrome.tabs.update(message.tabId, { "active": true }, function (tab) {
        if (chrome.runtime.lastError) {
          console.warn("Whoops... " + chrome.runtime.lastError.message);
        }
        chrome.windows.update(tab.windowId, { "focused": true });
      });
      return;
    case "moveTabs":
      var tabIds = message.tabIds;
      var index = message.index;
      var windowId = message.windowId;
      // inserting tabs when index != end is buggy so we move our tabs plus a bunch of tabs to the end instead
      chrome.windows.get(windowId, { "populate": true }, function (win) {
        var tabsAfterIndex = [];
        if (index != -1) {
          tabsAfterIndex = win.tabs.map(tab => tab.id).slice(index, win.tabs.length);
        }
        var tabsToMove = tabIds.concat(
          tabsAfterIndex.filter(function (tabId) {
            return !tabIds.includes(tabId);
          })
        );
        chrome.tabs.move(tabsToMove, { "index": -1, "windowId": windowId }, function () {
          if (chrome.runtime.lastError) {
            console.warn("Whoops... " + chrome.runtime.lastError.message);
          }
          sendTree();
          return;
        });
      });
      return true;
    case "openUrl":
      chrome.tabs.create({ "url": message.url });
      return;
    case "storeString":
      var blob = {};
      blob[message.name] = message.string;
      chrome.storage.local.set(blob);
      return;
  }
});

chrome.tabs.onActivated.addListener(function (activeInfo) {
  pushTo(visited, activeInfo.tabId);
});

chrome.windows.onFocusChanged.addListener(function (windowId) {
  if (windowId === chrome.windows.WINDOW_ID_NONE) {
    return;
  }
  chrome.tabs.query({ "active": true, "currentWindow": true, "windowType": "normal" }, function (tabs) {
    if (tabs.length > 0) {
      pushTo(visited, tabs[0].id);
    }
  });
});

var UPDATE_FROM_302 = "New look, the comeback of default selections, option to hide the browser panel, option to hide hints.";
var UPDATE_FROM_302_CONTEXT = "";

chrome.runtime.onInstalled.addListener(function (details) {
  if (details.reason == "install") {
    chrome.storage.sync.set({ "alwaysOnPanel": false, "hints": true });
  } else if (details.reason == "update") {
    if (isOlderThan(details.previousVersion, "3.1")) {
      chrome.storage.sync.set({ "alwaysOnPanel": true, "hints": true });
      var options = {
        type: "basic",
        title: "Keep It Tidy",
        message: UPDATE_FROM_302,
        contextMessage: UPDATE_FROM_302_CONTEXT,
        iconUrl: "img/logo-128.png"
      }
      chrome.notifications.create("update", options);
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