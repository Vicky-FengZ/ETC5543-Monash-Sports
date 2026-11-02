// Tests the real download script with in-memory browser APIs. This checks
// its behaviour without a network or browser; UI tests remain separate.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const script = fs.readFileSync(path.join(__dirname, '..', 'www', 'browser_downloads.js'), 'utf8');

function browser(pathname = '/app_test/') {
  const clicks = [];
  const downloads = [];
  const notifications = [];
  const blobs = new Map();
  const requests = [];
  let fetchResult = async () => ({
    ok: true,
    headers: { get: () => 'attachment; filename="cleaned-data.xlsx"' },
    blob: async () => ({ bytes: Buffer.from('sample file bytes') })
  });
  const Shiny = { notifications: { show: message => notifications.push(message) } };
  const window = { location: { pathname }, Shiny };
  const context = {
    window, Shiny, console,
    document: {
      addEventListener: (type, handler) => { if (type === 'click') clicks.push(handler); },
      body: { appendChild() {} },
      createElement: () => ({
        style: {}, remove() {},
        click() { downloads.push({ name: this.download, file: blobs.get(this.href) }); }
      })
    },
    fetch: href => { requests.push(href); return fetchResult(href); },
    URL: {
      createObjectURL: blob => { const url = 'blob:' + blobs.size; blobs.set(url, blob); return url; },
      revokeObjectURL: url => blobs.delete(url)
    },
    setTimeout: callback => callback()
  };
  vm.createContext(context);
  vm.runInContext(script, context);
  return {
    context, clicks, downloads, notifications, requests,
    respond: callback => { fetchResult = callback; },
    click(link) {
      const event = { target: { closest: () => link }, prevented: false,
        preventDefault() { this.prevented = true; }, stopPropagation() {} };
      for (const handler of clicks) handler(event);
      return event;
    }
  };
}

function link(disabled = false) {
  const attributes = new Map();
  return { href: '/app_test/session/download/file', dataset: {},
    classList: { contains: name => name === 'disabled' && disabled },
    getAttribute: name => name === 'download' ? 'fallback.rds' : attributes.get(name),
    setAttribute: (name, value) => attributes.set(name, value),
    removeAttribute: name => attributes.delete(name) };
}

async function settle() {
  for (let index = 0; index < 4; index++) await new Promise(setImmediate);
}

(async function() {
  const desktop = browser('/');
  assert.equal(desktop.clicks.length, 0, 'Regular Shiny downloads must remain untouched');

  const page = browser();
  const button = link();
  assert.equal(page.click(button).prevented, true);
  await settle();
  assert.equal(page.downloads[0].name, 'cleaned-data.xlsx');
  assert.equal(page.downloads[0].file.bytes.toString(), 'sample file bytes');
  assert.equal(button.dataset.downloading, undefined);
  assert.equal(button.getAttribute('aria-busy'), undefined);

  page.respond(async () => ({ ok: true,
    headers: { get: () => "attachment; filename*=UTF-8''plan%20%E6%B5%8B%E8%AF%95.csv" },
    blob: async () => ({ bytes: Buffer.from('month,income\nJan,100\n') }) }));
  page.click(button);
  await settle();
  assert.equal(page.downloads[1].name, 'plan \u6d4b\u8bd5.csv');

  const disabled = link(true);
  const requestsBefore = page.requests.length;
  page.click(disabled);
  assert.equal(page.requests.length, requestsBefore);
  assert.equal(disabled.dataset.downloading, undefined);

  let finish;
  page.respond(() => new Promise(resolve => { finish = resolve; }));
  page.click(button);
  page.click(button);
  assert.equal(page.requests.length, requestsBefore + 1, 'A second click must not duplicate a download');
  finish({ ok: true, headers: { get: () => null }, blob: async () => ({ bytes: Buffer.from('saved data') }) });
  await settle();
  assert.equal(page.downloads[2].name, 'fallback.rds');

  page.respond(async () => ({ ok: false, status: 500 }));
  page.click(button);
  await settle();
  assert.equal(page.downloads.length, 3);
  assert.match(page.notifications.at(-1).html, /HTTP 500/);
  assert.equal(button.dataset.downloading, undefined);

  page.respond(async () => { throw new Error('Connection lost'); });
  page.click(button);
  await settle();
  assert.match(page.notifications.at(-1).html, /Connection lost/);
  assert.equal(button.getAttribute('aria-busy'), undefined);
  assert.equal(button.dataset.downloading, undefined);

  vm.runInContext(script, page.context);
  assert.equal(page.clicks.length, 1, 'Reloading the script must not add duplicate click handlers');
  console.log('PASS browser download script: file bytes, filenames, disabled/double clicks, errors and retry state');
})().catch(error => { console.error(error); process.exitCode = 1; });
