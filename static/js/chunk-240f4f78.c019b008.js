(window["webpackJsonp"]=window["webpackJsonp"]||[]).push([["chunk-240f4f78"],{4056:function(e,t,s){"use strict";s.r(t);var o=function(){var e=this,t=e.$createElement,s=e._self._c||t;return s("div",{staticClass:"mt-2"},[s("div",{staticClass:"form-group form-row"},[s("div",{staticClass:"col-9"},[s("label",{staticClass:"bold",attrs:{for:"name"}},[e._v("Name")]),s("input",{directives:[{name:"model",rawName:"v-model",value:e.fields["name"],expression:"fields['name']"}],staticClass:"form-control",attrs:{type:"text",placeholder:"name"},domProps:{value:e.fields["name"]},on:{input:function(t){t.target.composing||e.$set(e.fields,"name",t.target.value)}}})]),s("div",{staticClass:"description_toggle col-3"},[s("div",{staticClass:"custom-control custom-switch"},[s("input",{staticClass:"custom-control-input",attrs:{id:"toggleDecription",type:"checkbox",showDescription:e.showDescription},on:{change:function(t){return e.toggle()}}}),s("label",{staticClass:"bold custom-control-label",attrs:{for:"toggleDecription"}},[e._v("Toggle description")])])])]),e.showDescription?s("div",{staticClass:"form-group"},[s("div",{staticClass:"p-0 col-12"},[s("label",{staticClass:"bold",attrs:{for:"description"}},[e._v("Decription")]),s("textarea",{directives:[{name:"model",rawName:"v-model",value:e.fields["description"],expression:"fields['description']"}],staticClass:"form-control",attrs:{placeholder:"description",cols:"30",rows:"3"},domProps:{value:e.fields["description"]},on:{input:function(t){t.target.composing||e.$set(e.fields,"description",t.target.value)}}})])]):e._e(),s("div",{staticClass:"form-group form-row"},[s("div",{class:"col-"+e.colWidth},[s("label",{staticClass:"bold"},[e._v("Cost per Package")]),s("input",{directives:[{name:"model",rawName:"v-model",value:e.fields["costPerPackage"],expression:"fields['costPerPackage']"}],staticClass:"form-control",attrs:{placeholder:"cost per package",type:"number",step:"0.01",min:"0"},domProps:{value:e.fields["costPerPackage"]},on:{input:function(t){t.target.composing||e.$set(e.fields,"costPerPackage",t.target.value)}}})]),s("div",{class:"col-"+e.colWidth},[s("label",{staticClass:"bold"},[e._v("Amount")]),s("input",{directives:[{name:"model",rawName:"v-model",value:e.fields["stockAmount"],expression:"fields['stockAmount']"}],staticClass:"form-control",attrs:{placeholder:"amount",type:"number",name:"amount"},domProps:{value:e.fields["stockAmount"]},on:{input:function(t){t.target.composing||e.$set(e.fields,"stockAmount",t.target.value)}}})]),"food"===e.activeTab?s("div",{class:"col-"+e.colWidth},[s("label",{staticClass:"bold"},[e._v("Calories per package")]),s("input",{directives:[{name:"model",rawName:"v-model",value:e.fields["caloriesPerPackage"],expression:"fields['caloriesPerPackage']"}],staticClass:"form-control",attrs:{placeholder:"Calories per package",type:"number"},domProps:{value:e.fields["caloriesPerPackage"]},on:{input:function(t){t.target.composing||e.$set(e.fields,"caloriesPerPackage",t.target.value)}}})]):e._e(),"water"===e.activeTab?s("div",{class:"col-"+e.colWidth},[s("label",{staticClass:"bold"},[e._v("Millilitre per package")]),s("input",{directives:[{name:"model",rawName:"v-model",value:e.fields["millilitrePerPackage"],expression:"fields['millilitrePerPackage']"}],staticClass:"form-control",attrs:{placeholder:"Millilitre per package",type:"number"},domProps:{value:e.fields["millilitrePerPackage"]},on:{input:function(t){t.target.composing||e.$set(e.fields,"millilitrePerPackage",t.target.value)}}})]):e._e()]),s("div",{staticClass:"form-group"},[s("button",{staticClass:"btn btn-secondary btn-block",attrs:{a:e.errors},on:{click:function(t){return e.create()}}},[e._v("Submit")])])])},i=[],a=s("5530"),r=s("2f62"),l={props:{activeTab:String},data:function(){return{fields:{name:"",stockAmount:"",costPerPackage:"",description:"",millilitrePerPackage:"",caloriesPerPackage:""},showDescription:!1}},methods:{toggle:function(){this.showDescription=!this.showDescription},create:function(){for(var e in this.$store.dispatch("submit",this.fields),this.fields)this.fields[e]=""}},mounted:function(){console.log(this.errors)},computed:Object(a["a"])(Object(a["a"])({},Object(r["b"])({errors:"errors"})),{},{colWidth:function(){var e="food"===this.activeTab||"water"===this.activeTab;return e?4:6}}),updated:function(){var e=this;this.errors?this.$swal({title:this.errors,icon:"error"}).then((function(){e.$store.commit("setError",null)})):console.log("no")}},c=l,n=(s("f49c"),s("2877")),d=Object(n["a"])(c,o,i,!1,null,"2a515e31",null);t["default"]=d.exports},"938b":function(e,t,s){},f49c:function(e,t,s){"use strict";var o=s("938b"),i=s.n(o);i.a}}]);
//# sourceMappingURL=chunk-240f4f78.c019b008.js.map