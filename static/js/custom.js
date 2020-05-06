window.onload = () => {
  const structure = [
    {id: 1, name: "File", icon: "fa fa-address-book fa-lg", children: [
      {id: 1.1, name: "Create", slug: "/app/create-form"},
    ]},
    {id: 2, name: "Categories", icon: "far fa-comment-dots fa-lg", children: [
      {id: 2.1, name: "Food", slug: "/app/list/food"},
      {id: 2.2, name: "Water", slug: "/app/list/water"},
      {id: 2.3, name: "Medicine", slug: "/app/list/medicine"},
      {id: 2.4, name: "Weapon", slug: "/app/list/weapon"},
    ]},
    {id: 3, name: "About", icon: "fa fa-indent fa-lg", children: [
      {id: 3.1, name: "About Quaremain", slug: "/about"}
    ]},
  ];
  
  Vue.component('sidebar-component', {
    data() {
      return {
        slug: "/",
        toggled: false,
        activeParent: null,
        activeParentCoordinates: null,
        structure,
        parentClasses: [
          "list-group-item",
          "d-flex",
          "justify-content-between",
          "align-items-center",
          "flex-row",
          "flex-nowrap",
        ],
      }
    },
    methods: {
      parentClick(parentID) {
        if(this.toggled) {
          this.toggled = false;
        }
        this.activeParent = this.activeParent === parentID ? null : parentID;
      },
      dynamicParentClasses(parentID) {
        return parentID === this.activeParent ? 
          [...this.parentClasses, 'active'] : [...this.parentClasses];
      },
      toggleSidebar() {
        this.toggled = !this.toggled;
      },
    },
    mounted() {
      const path = window.location.pathname;
  
      const activeParent = structure.find(parent => (
        parent.children.find(child => child.slug === path)
      ));
      if(activeParent) {
        this.parentClick(activeParent.id);
      }
  
      // this assures we have border-left on the active child
      this.slug = path;
    },
    updated() {
      console.log(this.slug)
    },  
    template: `
      <div :class="'sidebar ' + (toggled ? 'toggled' : '')">
        <li 
          @click="toggleSidebar()" 
          class="list-group-item box">
          <i
            class="fas fa-bars fa-2x"></i>
        </li>
        <ul class="list-group">
          <div
            class="parent_box"
            v-for="parent in structure"
            :key="parent.id"
          >
            <li 
              @click="parentClick(parent.id)"
              :class="dynamicParentClasses(parent.id)">
                <div>
                  <i :class="parent.icon"></i>
                  <a 
                    v-if="!toggled" 
                    class="parent_name">{{ parent.name }}</a>
                </div>
                <i  
                  v-if="!toggled" 
                  :class="'fa fa-lg fa-chevron-right ' + (parent.id === activeParent ? 'rotate' : '')"></i>
            </li>
            <ul class="list-group">
              <template v-if="parent.id === activeParent">
                <a
                  :key="child.id"
                  v-for="child in parent.children"
                  :href="child.slug">
                  <li 
                    :class="'list-group-item child ' + (child.slug === slug ? 'active-path' : '')">
                    <i v-if="!toggled" class="fa fa-chevron-right"></i>
                    <span>{{ child.name }}</span>
                  </li>
                </a>
              </template>
            </ul>
          </div>
        </ul>
      </div>
    `,
  });
  
  new Vue({ el: '#app'});
}