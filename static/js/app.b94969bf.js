(function(e){function t(t){for(var n,a,s=t[0],i=t[1],u=t[2],l=0,f=[];l<s.length;l++)a=s[l],Object.prototype.hasOwnProperty.call(c,a)&&c[a]&&f.push(c[a][0]),c[a]=0;for(n in i)Object.prototype.hasOwnProperty.call(i,n)&&(e[n]=i[n]);p&&p(t);while(f.length)f.shift()();return o.push.apply(o,u||[]),r()}function r(){for(var e,t=0;t<o.length;t++){for(var r=o[t],n=!0,a=1;a<r.length;a++){var s=r[a];0!==c[s]&&(n=!1)}n&&(o.splice(t--,1),e=i(i.s=r[0]))}return e}var n={},a={app:0},c={app:0},o=[];function s(e){return i.p+"js/"+({}[e]||e)+"."+{"chunk-0e58d61c":"42677858","chunk-239c7609":"07d8ebef","chunk-2d0a47c1":"408c836e","chunk-456e6120":"d90d8a70"}[e]+".js"}function i(t){if(n[t])return n[t].exports;var r=n[t]={i:t,l:!1,exports:{}};return e[t].call(r.exports,r,r.exports,i),r.l=!0,r.exports}i.e=function(e){var t=[],r={"chunk-239c7609":1,"chunk-456e6120":1};a[e]?t.push(a[e]):0!==a[e]&&r[e]&&t.push(a[e]=new Promise((function(t,r){for(var n="css/"+({}[e]||e)+"."+{"chunk-0e58d61c":"31d6cfe0","chunk-239c7609":"a6ad9c81","chunk-2d0a47c1":"31d6cfe0","chunk-456e6120":"28dbb540"}[e]+".css",c=i.p+n,o=document.getElementsByTagName("link"),s=0;s<o.length;s++){var u=o[s],l=u.getAttribute("data-href")||u.getAttribute("href");if("stylesheet"===u.rel&&(l===n||l===c))return t()}var f=document.getElementsByTagName("style");for(s=0;s<f.length;s++){u=f[s],l=u.getAttribute("data-href");if(l===n||l===c)return t()}var p=document.createElement("link");p.rel="stylesheet",p.type="text/css",p.onload=t,p.onerror=function(t){var n=t&&t.target&&t.target.src||c,o=new Error("Loading CSS chunk "+e+" failed.\n("+n+")");o.code="CSS_CHUNK_LOAD_FAILED",o.request=n,delete a[e],p.parentNode.removeChild(p),r(o)},p.href=c;var d=document.getElementsByTagName("head")[0];d.appendChild(p)})).then((function(){a[e]=0})));var n=c[e];if(0!==n)if(n)t.push(n[2]);else{var o=new Promise((function(t,r){n=c[e]=[t,r]}));t.push(n[2]=o);var u,l=document.createElement("script");l.charset="utf-8",l.timeout=120,i.nc&&l.setAttribute("nonce",i.nc),l.src=s(e);var f=new Error;u=function(t){l.onerror=l.onload=null,clearTimeout(p);var r=c[e];if(0!==r){if(r){var n=t&&("load"===t.type?"missing":t.type),a=t&&t.target&&t.target.src;f.message="Loading chunk "+e+" failed.\n("+n+": "+a+")",f.name="ChunkLoadError",f.type=n,f.request=a,r[1](f)}c[e]=void 0}};var p=setTimeout((function(){u({type:"timeout",target:l})}),12e4);l.onerror=l.onload=u,document.head.appendChild(l)}return Promise.all(t)},i.m=e,i.c=n,i.d=function(e,t,r){i.o(e,t)||Object.defineProperty(e,t,{enumerable:!0,get:r})},i.r=function(e){"undefined"!==typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(e,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(e,"__esModule",{value:!0})},i.t=function(e,t){if(1&t&&(e=i(e)),8&t)return e;if(4&t&&"object"===typeof e&&e&&e.__esModule)return e;var r=Object.create(null);if(i.r(r),Object.defineProperty(r,"default",{enumerable:!0,value:e}),2&t&&"string"!=typeof e)for(var n in e)i.d(r,n,function(t){return e[t]}.bind(null,n));return r},i.n=function(e){var t=e&&e.__esModule?function(){return e["default"]}:function(){return e};return i.d(t,"a",t),t},i.o=function(e,t){return Object.prototype.hasOwnProperty.call(e,t)},i.p="/",i.oe=function(e){throw console.error(e),e};var u=window["webpackJsonp"]=window["webpackJsonp"]||[],l=u.push.bind(u);u.push=t,u=u.slice();for(var f=0;f<u.length;f++)t(u[f]);var p=l;o.push([0,"chunk-vendors"]),r()})({0:function(e,t,r){e.exports=r("56d7")},"30e8":function(e,t,r){},"56d7":function(e,t,r){"use strict";r.r(t);r("e260"),r("e6cf"),r("cca6"),r("a79d");var n=r("2b0e"),a=function(){var e=this,t=e.$createElement,r=e._self._c||t;return r("div",{staticStyle:{display:"flex"}},[r("sidebar-component"),r("div",{staticClass:"experimental container pt-2 px-5 ml-2"},[r("div",{staticClass:"flex my-2"},[r("div",{staticClass:"form-group search_box"},[r("i",{staticClass:"fas fa-search fa-lg"}),r("input",{directives:[{name:"model",rawName:"v-model",value:e.search,expression:"search"}],staticClass:"pl-5 form-control",attrs:{id:"search",type:"text",placeholder:"Search"},domProps:{value:e.search},on:{input:function(t){t.target.composing||(e.search=t.target.value)}}})]),r("div",{staticClass:"form-group"},[r("button",{staticClass:"btn btn-secondary",attrs:{id:"create"},on:{click:function(t){return e.$store.commit("toggleShowForm")}}},[e._v("Create "+e._s(e.activeTab)+" "+e._s(e.search)+" ")])])]),e.showForm?[r("create-stock",{attrs:{activeTab:e.activeTab}})]:e._e(),r("hr"),r("ul",{staticClass:"nav nav-tabs"},e._l(["food","medicine","water","weapon"],(function(t){return r("li",{key:t,staticClass:"nav-item"},[r("a",{class:"nav-link "+(e.activeTab===t?"active":""),on:{click:function(r){return e.$store.dispatch("changeCategory",t)}}},[e._v(" "+e._s(t)+" ")])])})),0),r("table-component",{attrs:{stocks:e.stocks,search:e.search}})],2)],1)},c=[],o=(r("d3b7"),r("ac1f"),r("841c"),r("5530")),s=r("2f62"),i={components:{SidebarComponent:function(){return r.e("chunk-0e58d61c").then(r.bind(null,"5ea5"))},TableComponent:function(){return r.e("chunk-2d0a47c1").then(r.bind(null,"0748"))},CreateStock:function(){return r.e("chunk-239c7609").then(r.bind(null,"4056"))}},data:function(){return{}},mounted:function(){console.log(this.alert),this.$store.dispatch("loadStocks","food")},updated:function(){},computed:Object(o["a"])(Object(o["a"])({},Object(s["b"])({activeTab:"activeTab",showForm:"showForm",stocks:"stocks",alert:"alert"})),{},{search:{get:function(){return this.$store.state.search},set:function(e){this.$store.commit("updateSearch",e)}}}),methods:{showAlert:function(){this.$swal("Hello Vue world!!!")}}},u=i,l=r("2877"),f=Object(l["a"])(u,a,c,!1,null,"aff981fa",null),p=f.exports,d=(r("becf"),r("30e8"),r("7d05"),r("5886")),h=(r("4413"),r("4de4"),r("c975"),r("b0c0"),r("96cf"),r("1da1")),m=r("ade3"),v=(r("a4d3"),r("e01a"),r("99af"),r("d4ec")),g=r("bee2"),b=r("bc3a"),k=r.n(b),w="5000",y="http://localhost:".concat(w),O=.1,j="/api/".concat(O,"/app/"),P=k.a.create({baseURL:"".concat(y).concat(j)}),x=function(){function e(){Object(v["a"])(this,e)}return Object(g["a"])(e,[{key:"get",value:function(){var e=Object(h["a"])(regeneratorRuntime.mark((function e(t){return regeneratorRuntime.wrap((function(e){while(1)switch(e.prev=e.next){case 0:return e.abrupt("return",P.get("list/".concat(t)));case 1:case"end":return e.stop()}}),e)})));function t(t){return e.apply(this,arguments)}return t}()},{key:"remove",value:function(){var e=Object(h["a"])(regeneratorRuntime.mark((function e(t,r){return regeneratorRuntime.wrap((function(e){while(1)switch(e.prev=e.next){case 0:return e.abrupt("return",P.get("list/delete/".concat(t,"?stockCategory=").concat(r)));case 1:case"end":return e.stop()}}),e)})));function t(t,r){return e.apply(this,arguments)}return t}()},{key:"create",value:function(){var e=Object(h["a"])(regeneratorRuntime.mark((function e(t){return regeneratorRuntime.wrap((function(e){while(1)switch(e.prev=e.next){case 0:return e.abrupt("return",P.get("list/create",{params:t}));case 1:case"end":return e.stop()}}),e)})));function t(t){return e.apply(this,arguments)}return t}()},{key:"update",value:function(){var e=Object(h["a"])(regeneratorRuntime.mark((function e(t){var r,n;return regeneratorRuntime.wrap((function(e){while(1)switch(e.prev=e.next){case 0:return r={name:t["name"],description:t["description"],stockCategory:t["stockCategory"],stockAmount:t["amount"],costPerPackage:t["costPerPackage"]},"food"===t["stockCategory"]?r["caloriesPerPackage"]=t["caloriesPerPackage"]:"water"===t["stock-category"]&&(r["millilitre-per-package"]=t["millilitrePerPackage"]),n=t.id,console.log(r,n),e.abrupt("return",P.get("list/update/".concat(n),{params:r}));case 5:case"end":return e.stop()}}),e)})));function t(t){return e.apply(this,arguments)}return t}()}]),e}(),C=new x,S=function(){function e(){Object(v["a"])(this,e)}return Object(g["a"])(e,null,[{key:"submitData",value:function(e,t){var r={name:e["name"],description:e["description"],stockCategory:t,stockAmount:e["stockAmount"],costPerPackage:e["costPerPackage"]};return"water"===t&&(r["millilitrePerPackage"]=e["millilitrePerPackage"]),"food"===t&&(r["caloriesPerPackage"]=e["caloriesPerPackage"]),r}}]),e}();n["a"].use(s["a"]);var T=new s["a"].Store({state:{activeTab:"food",showForm:!1,stocks:{},search:"",alert:{a:5,active:!1}},mutations:{changeCategory:function(e,t){e.activeTab=t},toggleShowForm:function(e){e.showForm=!e.showForm},loadStocks:function(e,t){e.stocks=Object(o["a"])(Object(o["a"])({},e.stocks),{},Object(m["a"])({},e.activeTab,t))},updateSearch:function(e,t){e.search=t},setAlert:function(e,t){var r=t.active;console.log("set alert"),e.alert=Object(o["a"])(Object(o["a"])({},e.alert),{},{active:r})}},actions:{loadStocks:function(e,t){return Object(h["a"])(regeneratorRuntime.mark((function r(){var n;return regeneratorRuntime.wrap((function(r){while(1)switch(r.prev=r.next){case 0:return n=e.commit,r.t0=n,r.next=4,C.get(t);case 4:r.t1=r.sent.data.stocks,(0,r.t0)("loadStocks",r.t1);case 6:case"end":return r.stop()}}),r)})))()},changeCategory:function(e,t){return Object(h["a"])(regeneratorRuntime.mark((function r(){var n,a;return regeneratorRuntime.wrap((function(r){while(1)switch(r.prev=r.next){case 0:n=e.dispatch,a=e.commit,n("loadStocks",t),a("changeCategory",t);case 3:case"end":return r.stop()}}),r)})))()},submit:function(e,t){return Object(h["a"])(regeneratorRuntime.mark((function r(){var n,a,c,o,s,i;return regeneratorRuntime.wrap((function(r){while(1)switch(r.prev=r.next){case 0:return n=e.state,a=e.dispatch,c=e.commit,o=S.submitData(t,n.activeTab),r.next=4,C.create(o);case 4:if(s=r.sent,200!==s.status){r.next=14;break}if(i=404===s.data.status,i){r.next=10;break}return a("loadStocks",n.activeTab),r.abrupt("return");case 10:c("setAlert",{active:!0}),r.next=15;break;case 14:console.log("error");case 15:case"end":return r.stop()}}),r)})))()},removeStock:function(e,t){return Object(h["a"])(regeneratorRuntime.mark((function r(){var n,a,c;return regeneratorRuntime.wrap((function(r){while(1)switch(r.prev=r.next){case 0:return n=e.state,a=e.dispatch,r.next=3,C.remove(t,n.activeTab);case 3:c=r.sent,200===c.status?a("loadStocks",n.activeTab):console.log("error");case 5:case"end":return r.stop()}}),r)})))()},updateStock:function(e,t){return Object(h["a"])(regeneratorRuntime.mark((function r(){var n,a,c,o,s;return regeneratorRuntime.wrap((function(r){while(1)switch(r.prev=r.next){case 0:return n=e.state,a=e.dispatch,c=t.stock,o=t.name,c["name"]=o,c["stockCategory"]=n.activeTab,r.next=6,C.update(c);case 6:s=r.sent,200===s.status?a("loadStocks",n.activeTab):console.log("error");case 8:case"end":return r.stop()}}),r)})))()}},getters:{activeTab:function(e){var t=e.activeTab;return t},showForm:function(e){var t=e.showForm;return t},stocks:function(e){var t=e.stocks,r=e.search,n=e.activeTab;return r.length>0?t[n].filter((function(e){return e.name.toLowerCase().indexOf(r.toLowerCase())>-1})):t[n]},search:function(e){var t=e.search;return t},alert:function(e){var t=e.alert;return console.log("asd"),t}}});n["a"].config.productionTip=!1,n["a"].use(d["a"]),new n["a"]({store:T,render:function(e){return e(p)}}).$mount("#app")},"7d05":function(e,t,r){}});
//# sourceMappingURL=app.b94969bf.js.map