(window["webpackJsonp"]=window["webpackJsonp"]||[]).push([["chunk-043f97d0"],{"0300":function(t,s,a){"use strict";a.r(s);var e=function(){var t=this,s=t.$createElement,a=t._self._c||s;return a("tr",[a("td",[t._v(t._s(t.stock.id))]),a("td",[a("span",{staticClass:"c name",on:{click:function(s){return t.onUpdateClick("name","text")}}},[t._v(" "+t._s(t.stock.name)+" ")])]),a("td",{staticClass:"c",on:{click:function(s){return t.onUpdateClick("amount","number")}}},[t._v(" "+t._s(t.stock.amount)+" ")]),a("td",[t._v(t._s(t.stock.costPerPackage.toFixed(2)))]),"food"===t.$store.state.activeTab?a("td",[t._v(t._s(t.stock.caloriesPerPackage))]):t._e(),"water"===t.$store.state.activeTab?a("td",[t._v(t._s(t.stock.millilitrePerPackage)+"ML")]):t._e(),a("td",{staticClass:"actions"},[a("router-link",{staticClass:"fas fa-edit fa-lg",attrs:{tag:"i",to:"/stocks/"+t.$store.state.activeTab+"/"+t.stock.id}}),a("i",{staticClass:"fas fa-trash-alt fa-lg",on:{click:function(s){return t.$store.dispatch("removeStock",t.stock.id)}}})],1)])},c=[],n=(a("b0c0"),{data:function(){return{nameVal:"",updating:!1}},props:["stock"],methods:{onUpdateClick:function(t,s){var a={field:t,inputType:s,stock:this.stock};this.$store.commit("setUpdateObject",a)},nameClick:function(){this.updating=!this.updating,this.nameVal=this.stock.name}},mounted:function(){console.log(this.stock)}}),o=n,i=(a("4ac9"),a("2877")),r=Object(i["a"])(o,e,c,!1,null,"591e14d8",null);s["default"]=r.exports},"4ac9":function(t,s,a){"use strict";var e=a("bee7"),c=a.n(e);c.a},bee7:function(t,s,a){}}]);
//# sourceMappingURL=chunk-043f97d0.7be5a1b2.js.map