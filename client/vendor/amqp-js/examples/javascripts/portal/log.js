Ext.ux.LogPanel = Ext.extend(Ext.grid.GridPanel, {

	autoScroll	: true,

	data: [
		['Log Created', new Date()]
	],

	initComponent: function() {
	
		MQ.logger = this;
		
		this.store = new Ext.data.ArrayStore({
			fields: [
				{name: 'msg', type: 'string' },
				{name: 'when', type: 'date' }
			]
		});
		this.store.loadData(this.data);
		
		Ext.apply(this, {
			store: this.store,
			stripeRows: true,
			columns: [
				{header: "When", width: 50, sortable: true, dataIndex: 'when', renderer: Ext.util.Format.dateRenderer('h:i') },
				{header: "Message", id: "msg", sortable: false, dataIndex: 'msg'}
			],
			autoExpandColumn: 'msg',
			bbar: {
				items: [{
					xtype: 'button',
					text: "Clear Log",
					scope: this,
					handler: this.clear
				},"->",{
					xtype: "label",
					text: "Log Level: "
				},{
					xtype: 'button',
					text: "Debug",
					scope: this,
					handler: function() {
						this.update(1);
					}
				},{
					xtype: 'button',
					text: "Info",
					scope: this,
					handler: function() {
						this.update(2);
					}
				},{
					xtype: 'button',
					text: "Critical",
					scope: this,
					handler: function() {
						this.update(3);
					}
				}]
			}
		});
		
		Ext.ux.LogPanel.superclass.initComponent.apply(this, arguments);
	},
	
	
	
	log: function(msg){

		var record = new this.store.recordType({
			msg: msg,
			when: new Date()
		});
		
		this.store.add(record);
		this.scrollToBottom();
	},

	scrollToBottom: function() {
		var d = this.getView().scroller.dom;
		d.scrollTop = d.scrollHeight - d.offsetHeight;
		d.scrollLeft = 0;
	},
	
	clear: function(){
		this.data = [];
		this.store.loadData(this.data);
	},
	
	update: function(level){
		MQ.setLogLevel(level);
	},
	
	getLevel: function() {
		return this.getBottomToolbar().get("level").getValue();
	}
});
Ext.ComponentMgr.registerType('logPanel', Ext.ux.LogPanel);
