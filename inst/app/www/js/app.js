// ── KPI metric count-up + slide-up animation ─────────────────────────────────
(function () {
  'use strict';

  // track the last animated raw text per element so we don't repeat on same value
  const seen = new WeakMap();

  // parse "85.3 %" → { target: 85.3, suffix: "%", decimals: 1, raw: "85.3 %" }
  // returns null for values that should not be animated (dash, HH:MM clock)
  function parse(text) {
    text = (text || '').trim();
    if (!text || text === '—' || text === '-') return null;
    if (/^\d{2}:\d{2}$/.test(text)) return null; // HH:MM clock metric — skip
    const m = text.match(/^(\d+(?:\.\d+)?)\s*(.*)$/);
    if (!m) return null;
    return {
      target: parseFloat(m[1]),
      suffix: m[2].trim(),
      decimals: (m[1].split('.')[1] || '').length,
      raw: text
    };
  }

  function fmt(parsed, v) {
    return v.toFixed(parsed.decimals) + (parsed.suffix ? ' ' + parsed.suffix : '');
  }

  function countUp(el) {
    const parsed = parse(el.textContent);
    if (!parsed) return;
    if (seen.get(el) === parsed.raw) return; // same value — no re-animation
    seen.set(el, parsed.raw);

    const { target } = parsed;
    const duration = 850; // ms
    const start = performance.now();
    const ease = function (t) { return 1 - Math.pow(1 - t, 3); }; // cubic ease-out

    // sibling progress bar fill inside the same .metric-card (if any)
    const card = el.closest('.metric-card');
    const fill = card ? card.querySelector('.metric-progress-fill') : null;
    const fillTarget = fill ? parseFloat(fill.style.width) : null;

    // start state: value shifted down + invisible; bar at 0
    el.style.transition = 'none';
    el.style.transform = 'translateY(10px)';
    el.style.opacity = '0';
    el.textContent = fmt(parsed, 0);
    if (fill !== null) {
      fill.style.transition = 'none';
      fill.style.width = '0%';
    }

    requestAnimationFrame(function frame(now) {
      var t = Math.min((now - start) / duration, 1);
      var e = ease(t);
      el.textContent = fmt(parsed, target * e);
      el.style.transform = 'translateY(' + (10 * (1 - e)).toFixed(2) + 'px)';
      el.style.opacity = e.toFixed(3);
      if (fill !== null) {
        fill.style.width = (fillTarget * e).toFixed(2) + '%';
      }
      if (t < 1) {
        requestAnimationFrame(frame);
      } else {
        el.textContent = parsed.raw; // restore exact server-rendered text
        el.style.transform = '';
        el.style.opacity = '';
        el.style.transition = '';
        if (fill !== null) {
          fill.style.width = fillTarget + '%';
          fill.style.transition = '';
        }
      }
    });
  }

  function scanAndAnimate(root) {
    var els = root.querySelectorAll ? root.querySelectorAll('.metric') : [];
    for (var i = 0; i < els.length; i++) countUp(els[i]);
  }

  var observer = new MutationObserver(function (mutations) {
    for (var i = 0; i < mutations.length; i++) {
      var added = mutations[i].addedNodes;
      for (var j = 0; j < added.length; j++) {
        var node = added[j];
        if (node.nodeType !== 1) continue;
        if (node.classList && node.classList.contains('metric')) {
          countUp(node);
        } else {
          scanAndAnimate(node);
        }
      }
    }
  });

  document.addEventListener('DOMContentLoaded', function () {
    observer.observe(document.body, { childList: true, subtree: true });
  });
}());


function add_loading_spinner(btn_id, msg = "Loading...") {
  let inner_html =
    '<span class="spinner-border spinner-border-sm" role="status" aria-hidden="true"></span>' +
    `<span class="ps-1">${msg}</span>`;
  let button = $("#" + btn_id);
  button.html(inner_html);
  button.prop("disabled", true);
}

function remove_loading_spinner(btn_id, inner_html) {
  let button = $("#" + btn_id);
  button.html(inner_html);
  button.prop("disabled", false);
}

Shiny.addCustomMessageHandler("remove_loading_spinner", (msg) => {
  remove_loading_spinner(msg.btn_id, msg.inner_html);
});
