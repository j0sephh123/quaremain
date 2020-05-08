window.onload = () => {
  const config = {
    API_VER: 0.1,
  }

  const structure = [
    {id: 1, name: "File", icon: "far fa-file-alt fa-lg", children: [
      {id: 1.1, name: "Create", slug: "/app/create-form"},
    ]},
    {id: 2, name: "Categories", icon: "far fa-list-alt fa-lg", children: [
      {id: 2.1, name: "Food", slug: "/app/list/food"},
      {id: 2.2, name: "Water", slug: "/app/list/water"},
      {id: 2.3, name: "Medicine", slug: "/app/list/medicine"},
      {id: 2.4, name: "Weapon", slug: "/app/list/weapon"},
    ]},
    {id: 3, name: "About", icon: "fas fa-info-circle fa-lg", children: [
      {id: 3.1, name: "About Quaremain", slug: "/about"}
    ]},
    {id: 4, name: "Experimental", icon: "fas fa-atom fa-lg", children: [
      {id: 4.1, name: "Index", slug: "/experimental"}
    ]},
  ];
  
  Vue.component('sidebar-component', {
    data() {
      return {
        slug: "/",
        toggled: true,
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
      //console.log('updated', {slug: this.slug})
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

  Vue.component('stock-component', {
    data() {
      return {
        search: "",
        loaded: false,
        categories: ["food", "medicine", "water", "weapon"],
        activeCategory: "food",
        stocks: {},
      }
    },
    template: `
      <div class="experimental">

        <div class="flex my-2">
          <div class="form-group search_box">
            <i class="fas fa-search fa-lg"></i>
            <input  
              v-model="search"
              id="search"
              type="text"
              placeholder="Search"
              class="pl-5 form-control" />
          </div>
          <div class="form-group">
            <button id="create" class="btn btn-secondary"
              >Create {{ activeCategory }} {{ search }}
            </button>
          </div>
        </div>

        <ul class="nav nav-tabs">
          <li 
            :key="category"
            v-for="category in categories"
            class="nav-item">
            <a 
              @click="changeCategory(category)"
              :class="'nav-link ' + (activeCategory === category ? 'active' : '')">
              {{ category }}
            </a>
          </li>
        </ul>
        <table class="table table-bordered">
          <thead class="thead-light">
            <tr>
              <th>ID</th>
              <th>Name</th>
              <th>Description</th>
              <th>Amount</th>
              <th>Total Cost</th>
              <th v-if="activeCategory === 'water'">Total ML (Millilitre)</th>
              <th v-if="activeCategory === 'food'">Total Potential Calories</th>
              <th>Actions</th>
            </tr>
            <template v-if="loaded">
              <tr
                :key="stock.id"
                v-for="stock in activeStocks"
              >
                <td>{{ stock.id }}</td>
                <td>{{ stock.name }}</td>
                <td>{{ stock.description }}</td>
                <td>{{ stock.amount }}</td>
                <td>\${{ (stock.amount * stock.costPerPackage).toFixed(2) }}</td>
                <td v-if="activeCategory === 'food'">{{ stock.caloriesPerPackage }}</td>
                <td v-if="activeCategory === 'water'">{{ stock.millilitrePerPackage }}ML</td>
                <td class="actions">
                  <i 
                    @click="removeStockItem(stock.id, activeCategory)"
                    class="fas fa-trash-alt"></i>
                </td>
              </tr>
            </template>
          </thead>
        </table>
      </div>
    `,
    mounted() {
      // we have to start somewhere
      this.get('food');
    },
    updated() {
      
    },
    computed: {
      activeStocks() {
        if(this.search.length > 0) {
          return this.stocks[this.activeCategory].filter(item => (
            item.name.toLowerCase().indexOf(this.search.toLowerCase()) > -1
          ));
        } 
        return this.stocks[this.activeCategory];
      }
    },
    methods: {
      removeStockItem(id, collection) {
        fetch(`/api/${config.API_VER}/app/list/delete/${id}?stock-category=${collection}`)
          .then(result => result.json())
          .then(data => {
            this.get(collection);
            this.loaded = false;
          })
          .catch(error => {
            console.log(error);
          })
      },
      changeCategory(category) {
        this.activeCategory = category;
        this.get(category);
        this.loaded = false;
        this.search = "";
      },
      get(collection) {
        fetch(`/api/${config.API_VER}/app/list/${collection}`)
          .then(result => result.json())
          .then(data => {
            this.loaded = true;
            this.stocks[collection] = data.stocks;
          })
          .catch(error => {
            console.log(error);
          })
      }
    }
  });

  new Vue({ el: '#experimental'});
  new Vue({ el: '#sidebar'});
}