(function(e){function t(t){for(var r,o,u=t[0],s=t[1],i=t[2],l=0,p=[];l<u.length;l++)o=u[l],Object.prototype.hasOwnProperty.call(a,o)&&a[o]&&p.push(a[o][0]),a[o]=0;for(r in s)Object.prototype.hasOwnProperty.call(s,r)&&(e[r]=s[r]);f&&f(t);while(p.length)p.shift()();return c.push.apply(c,i||[]),n()}function n(){for(var e,t=0;t<c.length;t++){for(var n=c[t],r=!0,o=1;o<n.length;o++){var s=n[o];0!==a[s]&&(r=!1)}r&&(c.splice(t--,1),e=u(u.s=n[0]))}return e}var r={},a={app:0},c=[];function o(e){return u.p+"js/"+({}[e]||e)+"."+{"chunk-1a07a938":"cef97d83","chunk-2d0ae8c8":"250efdce","chunk-2d0b1a62":"71c89ab4","chunk-2d0b3289":"76e90043","chunk-2d0c5405":"502ee13d","chunk-2d0d093a":"51e7f9ae","chunk-2d22d746":"59f74e3a","chunk-4de24aae":"4b73fc4f","chunk-2d0a47c1":"8bb1dcc4","chunk-2d0bff13":"b80dc1a1","chunk-2d0e5f91":"6305c48f","chunk-095c89a4":"cde0c97b","chunk-2d0a3839":"402d129d"}[e]+".js"}function u(t){if(r[t])return r[t].exports;var n=r[t]={i:t,l:!1,exports:{}};return e[t].call(n.exports,n,n.exports,u),n.l=!0,n.exports}u.e=function(e){var t=[],n=a[e];if(0!==n)if(n)t.push(n[2]);else{var r=new Promise((function(t,r){n=a[e]=[t,r]}));t.push(n[2]=r);var c,s=document.createElement("script");s.charset="utf-8",s.timeout=120,u.nc&&s.setAttribute("nonce",u.nc),s.src=o(e);var i=new Error;c=function(t){s.onerror=s.onload=null,clearTimeout(l);var n=a[e];if(0!==n){if(n){var r=t&&("load"===t.type?"missing":t.type),c=t&&t.target&&t.target.src;i.message="Loading chunk "+e+" failed.\n("+r+": "+c+")",i.name="ChunkLoadError",i.type=r,i.request=c,n[1](i)}a[e]=void 0}};var l=setTimeout((function(){c({type:"timeout",target:s})}),12e4);s.onerror=s.onload=c,document.head.appendChild(s)}return Promise.all(t)},u.m=e,u.c=r,u.d=function(e,t,n){u.o(e,t)||Object.defineProperty(e,t,{enumerable:!0,get:n})},u.r=function(e){"undefined"!==typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(e,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(e,"__esModule",{value:!0})},u.t=function(e,t){if(1&t&&(e=u(e)),8&t)return e;if(4&t&&"object"===typeof e&&e&&e.__esModule)return e;var n=Object.create(null);if(u.r(n),Object.defineProperty(n,"default",{enumerable:!0,value:e}),2&t&&"string"!=typeof e)for(var r in e)u.d(n,r,function(t){return e[t]}.bind(null,r));return n},u.n=function(e){var t=e&&e.__esModule?function(){return e["default"]}:function(){return e};return u.d(t,"a",t),t},u.o=function(e,t){return Object.prototype.hasOwnProperty.call(e,t)},u.p="/",u.oe=function(e){throw console.error(e),e};var s=window["webpackJsonp"]=window["webpackJsonp"]||[],i=s.push.bind(s);s.push=t,s=s.slice();for(var l=0;l<s.length;l++)t(s[l]);var f=i;c.push([0,"chunk-vendors"]),n()})({0:function(e,t,n){e.exports=n("56d7")},"30e8":function(e,t,n){},3338:function(e,t,n){"use strict";n.d(t,"a",(function(){return m}));n("a4d3"),n("e01a"),n("99af"),n("b0c0"),n("d3b7"),n("96cf");var r=n("1da1"),a=n("d4ec"),c=n("bee2"),o=n("bc3a"),u=n.n(o),s=n("41cb"),i=n("5fb0"),l="5000",f="http://localhost:".concat(l),p="/api/app/",d=u.a.create({baseURL:"".concat(f).concat(p)}),h=function(){function e(){Object(a["a"])(this,e)}return Object(c["a"])(e,[{key:"delay",value:function(){var e=Object(r["a"])(regeneratorRuntime.mark((function e(t){return regeneratorRuntime.wrap((function(e){while(1)switch(e.prev=e.next){case 0:return e.abrupt("return",new Promise((function(e){return setTimeout(e,t)})));case 1:case"end":return e.stop()}}),e)})));function t(t){return e.apply(this,arguments)}return t}()},{key:"getRandomNum",value:function(e,t){return Math.floor(Math.random()*(t-e)+e)}},{key:"get",value:function(){var e=Object(r["a"])(regeneratorRuntime.mark((function e(t){return regeneratorRuntime.wrap((function(e){while(1)switch(e.prev=e.next){case 0:return e.abrupt("return",d.get("list/".concat(t)));case 1:case"end":return e.stop()}}),e)})));function t(t){return e.apply(this,arguments)}return t}()},{key:"getOne",value:function(){var e=Object(r["a"])(regeneratorRuntime.mark((function e(t,n){var r,a;return regeneratorRuntime.wrap((function(e){while(1)switch(e.prev=e.next){case 0:return e.next=2,d.get("list/show/".concat(n,"?stockCategory=").concat(t));case 2:if(r=e.sent.data.stock,r){e.next=5;break}return e.abrupt("return",!1);case 5:return a=r[0],e.abrupt("return",a);case 7:case"end":return e.stop()}}),e)})));function t(t,n){return e.apply(this,arguments)}return t}()},{key:"remove",value:function(){var e=Object(r["a"])(regeneratorRuntime.mark((function e(t,n){return regeneratorRuntime.wrap((function(e){while(1)switch(e.prev=e.next){case 0:return e.abrupt("return",d.get("list/delete/".concat(t,"?stockCategory=").concat(n)));case 1:case"end":return e.stop()}}),e)})));function t(t,n){return e.apply(this,arguments)}return t}()},{key:"create",value:function(){var e=Object(r["a"])(regeneratorRuntime.mark((function e(t){return regeneratorRuntime.wrap((function(e){while(1)switch(e.prev=e.next){case 0:return e.abrupt("return",d.get("list/create",{params:t}));case 1:case"end":return e.stop()}}),e)})));function t(t){return e.apply(this,arguments)}return t}()},{key:"update",value:function(){var e=Object(r["a"])(regeneratorRuntime.mark((function e(t){var n;return regeneratorRuntime.wrap((function(e){while(1)switch(e.prev=e.next){case 0:return n={name:t["name"],description:t["description"],stockCategory:t["stockCategory"],stockAmount:t["amount"],costPerPackage:t["costPerPackage"]},"food"===t["stockCategory"]?n["caloriesPerPackage"]=t["caloriesPerPackage"]:"water"===t["stockCategory"]&&(n["millilitrePerPackage"]=t["millilitrePerPackage"]),e.abrupt("return",d.get("list/update/".concat(t.id),{params:n}));case 3:case"end":return e.stop()}}),e)})));function t(t){return e.apply(this,arguments)}return t}()},{key:"resetDatabase",value:function(){var e=Object(r["a"])(regeneratorRuntime.mark((function e(){var t;return regeneratorRuntime.wrap((function(e){while(1)switch(e.prev=e.next){case 0:return e.next=2,d.get("list/reset-database");case 2:t=e.sent.data.status,t===i["a"].success?s["a"].push({name:"App"}):console.log("error");case 4:case"end":return e.stop()}}),e)})));function t(){return e.apply(this,arguments)}return t}()},{key:"getSurvival",value:function(){var e=Object(r["a"])(regeneratorRuntime.mark((function e(){var t,n;return regeneratorRuntime.wrap((function(e){while(1)switch(e.prev=e.next){case 0:return t="list/total-survival-days",e.next=3,d.get(t);case 3:if(n=e.sent.data,n.status!==i["a"].notFound){e.next=6;break}return e.abrupt("return",{text:"".concat(n.error),alertClassName:"danger"});case 6:return e.abrupt("return",{text:"".concat(n.totalSurvivalDays," survival days left."),alertClassName:n.survivalAlertType});case 7:case"end":return e.stop()}}),e)})));function t(){return e.apply(this,arguments)}return t}()},{key:"getAllStocks",value:function(){var e=Object(r["a"])(regeneratorRuntime.mark((function e(){var t,n;return regeneratorRuntime.wrap((function(e){while(1)switch(e.prev=e.next){case 0:return t="list/get-all-stocks",e.next=3,d.get(t);case 3:if(n=e.sent.data,n.status!==i["a"].success){e.next=6;break}return e.abrupt("return",n.stocks);case 6:throw new Error("Error in fetching all stocks in api.getAllStocks");case 7:case"end":return e.stop()}}),e)})));function t(){return e.apply(this,arguments)}return t}()}]),e}(),m=new h},3597:function(e,t,n){},"41cb":function(e,t,n){"use strict";n("d3b7");var r=n("2b0e"),a=n("8c4f");r["a"].use(a["a"]),t["a"]=new a["a"]({mode:"history",base:"/",routes:[{path:"/",name:"App",component:function(){return n.e("chunk-2d0c5405").then(n.bind(null,"3dfd"))}},{path:"/survival",name:"Survival days",component:function(){return n.e("chunk-2d0ae8c8").then(n.bind(null,"0b11"))}},{path:"/charts",name:"Charts",component:function(){return n.e("chunk-2d0d093a").then(n.bind(null,"6923"))}},{path:"/settings",name:"Settings",component:function(){return n.e("chunk-2d0b3289").then(n.bind(null,"26d3"))}},{path:"/about",name:"About",component:function(){return n.e("chunk-2d22d746").then(n.bind(null,"f820"))}},{path:"/stocks/:stock/:id",name:"Stock",component:function(){return n.e("chunk-4de24aae").then(n.bind(null,"68e3"))}}]})},"56d7":function(e,t,n){"use strict";n.r(t);n("e260"),n("e6cf"),n("cca6"),n("a79d");var r=n("2b0e"),a=n("5886"),c=(n("becf"),n("30e8"),n("4413"),n("41cb")),o=n("2f62"),u={field:null,inputType:null,stock:null,active:!1},s={theme:{current:"minty",list:["minty","lux"]},errors:null,activeTab:"food",showForm:!1,stocks:{},search:"",updateObject:u,oneStock:null,sidebar:{structure:[{id:1,name:"Quaremain",icon:"fas fa-atom",slug:"/"},{id:5,name:"Charts",icon:"fas fa-chart-pie",slug:"/charts"},{id:2,name:"About",icon:"fas fa-info-circle",slug:"/about"},{id:3,name:"Settings",icon:"fas fa-cog",slug:"/settings"},{id:4,name:"Survival days",icon:"fas fa-calendar-alt",slug:"/survival"}]}},i=(n("4de4"),n("c975"),n("b0c0"),n("ac1f"),n("841c"),{activeTab:function(e){var t=e.activeTab;return t},showForm:function(e){var t=e.showForm;return t},stocks:function(e){var t=e.stocks,n=e.search,r=e.activeTab;return n.length>0?t[r].filter((function(e){return e.name.toLowerCase().indexOf(n.toLowerCase())>-1})):t[r]},search:function(e){var t=e.search;return t},updateObject:function(e){var t=e.updateObject;return t},errors:function(e){var t=e.errors;return t},stock:function(e){var t=e.oneStock;return t},sidebarItems:function(e){var t=e.sidebar;return t.structure},theme:function(e){var t=e.theme;return t}}),l=n("ade3"),f=n("5530"),p={changeCategory:function(e,t){e.activeTab=t},toggleShowForm:function(e){e.showForm=!e.showForm},loadStocks:function(e,t){e.stocks=Object(f["a"])(Object(f["a"])({},e.stocks),{},Object(l["a"])({},e.activeTab,t))},updateSearch:function(e,t){e.search=t},setUpdateObject:function(e,t){e.updateObject=t?Object(f["a"])({active:!0},t):u},setError:function(e,t){e.errors=t},setOneStock:function(e,t){e.oneStock=t},setCurrentTheme:function(e,t){e.theme.current=t}},d=(n("96cf"),n("1da1")),h=n("3338"),m=(n("a4d3"),n("e01a"),n("262e")),b=n("2caf"),v=n("d4ec"),g=function e(t){Object(v["a"])(this,e),this.name=t.name,this.description=t.description,this.stockAmount=t.stockAmount,this.costPerPackage=t.costPerPackage,this.stockCategory=t.stockCategory},k=function(e){Object(m["a"])(n,e);var t=Object(b["a"])(n);function n(e){var r;return Object(v["a"])(this,n),r=t.call(this,e),r.millilitrePerPackage=e.millilitrePerPackage,r}return n}(g),w=function(e){Object(m["a"])(n,e);var t=Object(b["a"])(n);function n(e){var r;return Object(v["a"])(this,n),r=t.call(this,e),r.caloriesPerPackage=e.caloriesPerPackage,r}return n}(g),y=function(e,t){return t["stockCategory"]=e,"food"===e?new w(t):"water"===e?new k(t):new g(t)},O=n("5fb0"),j={loadStocks:function(e,t){return Object(d["a"])(regeneratorRuntime.mark((function n(){var r;return regeneratorRuntime.wrap((function(n){while(1)switch(n.prev=n.next){case 0:return r=e.commit,n.t0=r,n.next=4,h["a"].get(t);case 4:n.t1=n.sent.data.stocks,(0,n.t0)("loadStocks",n.t1);case 6:case"end":return n.stop()}}),n)})))()},changeCategory:function(e,t){return Object(d["a"])(regeneratorRuntime.mark((function n(){var r,a;return regeneratorRuntime.wrap((function(n){while(1)switch(n.prev=n.next){case 0:r=e.dispatch,a=e.commit,r("loadStocks",t),a("changeCategory",t);case 3:case"end":return n.stop()}}),n)})))()},submit:function(e,t){return Object(d["a"])(regeneratorRuntime.mark((function n(){var r,a,c,o,u,s;return regeneratorRuntime.wrap((function(n){while(1)switch(n.prev=n.next){case 0:return r=e.state,a=e.dispatch,c=e.commit,console.log(r.activeTab,t),o=y(r.activeTab,t),n.next=5,h["a"].create(o);case 5:u=n.sent,u.status===O["a"].success?(s=u.data.status===O["a"].notFound,s?c("setError",u.data.error):a("loadStocks",r.activeTab)):console.log("Error with creating a stock.");case 7:case"end":return n.stop()}}),n)})))()},removeStock:function(e,t){return Object(d["a"])(regeneratorRuntime.mark((function n(){var r,a,c;return regeneratorRuntime.wrap((function(n){while(1)switch(n.prev=n.next){case 0:return r=e.state,a=e.dispatch,n.next=3,h["a"].remove(t,r.activeTab);case 3:c=n.sent,c.status===O["a"].success?a("loadStocks",r.activeTab):console.log("error loading stocks");case 5:case"end":return n.stop()}}),n)})))()},updateStock:function(e,t){return Object(d["a"])(regeneratorRuntime.mark((function n(){var r,a,c,o,u,s;return regeneratorRuntime.wrap((function(n){while(1)switch(n.prev=n.next){case 0:return r=e.state,a=e.dispatch,c=t.stock,o=t.field,u=t.value,c[o]=u,c["stockCategory"]=r.activeTab,n.next=6,h["a"].update(c);case 6:s=n.sent,s.status===O["a"].success?a("loadStocks",r.activeTab):console.log("error");case 8:case"end":return n.stop()}}),n)})))()},update:function(e,t){return Object(d["a"])(regeneratorRuntime.mark((function n(){var r,a,c,o;return regeneratorRuntime.wrap((function(n){while(1)switch(n.prev=n.next){case 0:r=e.dispatch,a=e.state,c=e.commit,o={stock:a.updateObject.stock,field:a.updateObject.field,value:t},c("setUpdateObject",null),r("updateStock",o);case 4:case"end":return n.stop()}}),n)})))()},getOneStock:function(e,t){return Object(d["a"])(regeneratorRuntime.mark((function n(){var r,a,o,u;return regeneratorRuntime.wrap((function(n){while(1)switch(n.prev=n.next){case 0:return r=e.commit,a=t.stock,o=t.id,n.next=4,h["a"].getOne(a,o);case 4:u=n.sent,u?r("setOneStock",u):c["a"].push({name:"App"});case 6:case"end":return n.stop()}}),n)})))()},resetDatabase:function(){return Object(d["a"])(regeneratorRuntime.mark((function e(){return regeneratorRuntime.wrap((function(e){while(1)switch(e.prev=e.next){case 0:return e.next=2,h["a"].resetDatabase();case 2:case"end":return e.stop()}}),e)})))()}};r["a"].use(o["a"]);var x=new o["a"].Store({state:s,mutations:p,actions:j,getters:i}),R=function(){var e=this,t=e.$createElement,n=e._self._c||t;return n("div",{class:"theme_"+e.$store.getters.theme.current},[n("nav-component"),n("div",{staticStyle:{display:"flex"}},[n("sidebar-component"),n("router-view")],1)],1)},S=[],P=(n("d3b7"),{components:{SidebarComponent:function(){return n.e("chunk-1a07a938").then(n.bind(null,"5ea5"))},NavComponent:function(){return n.e("chunk-2d0b1a62").then(n.bind(null,"216c"))}}}),C=P,T=(n("5f9a"),n("2877")),A=Object(T["a"])(C,R,S,!1,null,null,null),E=A.exports;r["a"].use(a["a"]),r["a"].config.productionTip=!1,new r["a"]({router:c["a"],store:x,render:function(e){return e(E)}}).$mount("#app")},"5f9a":function(e,t,n){"use strict";var r=n("3597"),a=n.n(r);a.a},"5fb0":function(e,t,n){"use strict";n.d(t,"a",(function(){return r}));var r={success:200,notFound:404}}});
//# sourceMappingURL=app.6fcd6fd8.js.map