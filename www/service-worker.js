// Service worker for Tarot Journal PWA.
// Network-first for navigations (Shiny needs a live connection anyway),
// with a cached offline fallback page.

const OFFLINE_VERSION = 1;
const CACHE_NAME = "tarot-offline-v" + OFFLINE_VERSION;
const OFFLINE_URL = "offline.html";

self.addEventListener("install", (event) => {
  event.waitUntil(
    (async () => {
      const cache = await caches.open(CACHE_NAME);
      await cache.add(new Request(OFFLINE_URL, { cache: "reload" }));
    })()
  );
  self.skipWaiting();
});

self.addEventListener("activate", (event) => {
  event.waitUntil(
    (async () => {
      if ("navigationPreload" in self.registration) {
        await self.registration.navigationPreload.enable();
      }
      // Clean up old caches
      const keys = await caches.keys();
      await Promise.all(
        keys.filter((k) => k !== CACHE_NAME).map((k) => caches.delete(k))
      );
    })()
  );
  self.clients.claim();
});

self.addEventListener("fetch", (event) => {
  // Only intercept navigation requests; let everything else
  // (websocket, assets, API calls) go straight to the network.
  if (event.request.mode === "navigate") {
    event.respondWith(
      (async () => {
        try {
          const preloadResponse = await event.preloadResponse;
          if (preloadResponse) return preloadResponse;
          return await fetch(event.request);
        } catch (error) {
          const cache = await caches.open(CACHE_NAME);
          return await cache.match(OFFLINE_URL);
        }
      })()
    );
  }
});
