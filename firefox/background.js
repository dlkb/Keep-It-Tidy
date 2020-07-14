const MAX_LENGTH = 1024; // max length of the array keeping track of the browsing history
var visited = []; // tabIds of the last visited tabs, [last, next-to-last, ...]

function pushTo(arr, item) {
  arr.unshift(item);
  if (arr.length > MAX_LENGTH) {
    arr.pop();
  }
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
      getWindowsJson(function (json) {
        sendResponse({ "windows": json, "visited": visited });
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
    case "extractTabs":
      var tabIds = message.tabIds;
      var firstTabId = tabIds.shift();
      browser.windows.create({ "tabId": firstTabId }, function (win) {
        if (tabIds.length == 0) {
          return;
        }
        browser.tabs.move(tabIds, { "windowId": win.id, "index": 1 }, function () {
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
            count++;
            if (browser.runtime.lastError) {
              console.warn("Whoops... " + browser.runtime.lastError.message);
              sendTree();
              return;
            }
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
                count++;
                if (count == Object.keys(tabIdsOf).length) {
                  sendTree();
                  return;
                }
                if (chrome.runtime.lastError) {
                  console.warn("Whoops... " + chrome.runtime.lastError.message);
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
      browser.tabs.update(message.tabId, { "active": true });
      browser.windows.update(message.windowId, { "focused": true });
      return;
    case "moveTabs":
      var tabIds = message.tabIds;
      var index = message.index;
      var count = 0;
      if (message.index == -1) {
        tabIds.reverse(); // trick
      }
      for (let tabId of tabIds) { // inserting a group of tabs at once doesn't work in this case, tabs end up not together, not sure why
        browser.tabs.move(tabId, { "index": index, "windowId": message.windowId }, function () {
          count++;
          if (browser.runtime.lastError) {
            console.warn("Whoops... " + browser.runtime.lastError.message);
            sendTree();
            return;
          }
          if (count == tabIds.length) {
            sendTree();
            return;
          }
        });
      }
      return true;
    case "openUrl":
      browser.tabs.create({ "url": message.url });
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

var UPDATE_FROM_224 = "New name, new logo, simpler design, bug fixes. See the store for more info.";
var UPDATE_FROM_224_CONTEXT = "Top Tomato => Keep It Tidy"

browser.runtime.onInstalled.addListener(function (details) {
  if (details.reason == "install") { // if first installation
    // Set default values for options
  } else if (details.reason == "update") { // if update of the extension
    // Say something to the user
    if (details.previousVersion == "2.2.4") {
      var options = {
        type: "basic",
        title: "Keep It Tidy",
        message: UPDATE_FROM_224,
        contextMessage: UPDATE_FROM_224_CONTEXT,
        iconUrl: "img/logo-128.png"
      }
      browser.notifications.create("update", options);
    }
  }
});
