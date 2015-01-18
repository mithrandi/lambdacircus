class Quote extends Backbone.Model
    voteUp: =>
        d = $.post(@get('voteUp'), undefined, undefined, 'json')
        d.done (data) =>
            @voted = true
            @set data

    voteDown: =>
        d = $.post(@get('voteDown'), undefined, undefined, 'json')
        d.done (data) =>
            @voted = true
            @set data

    url: =>
        @get('self')


class QuoteView extends Backbone.View
    className: 'quote-container'
    tagName: 'div'
    template: _.template $('#template-quote').html()
    events:
        'click button.vote-up':       'voteUp'
        'click button.vote-down':     'voteDown'
        'click button.action-remove': 'actionRemove'

    initialize: ->
        @model.on 'change', @render
        @model.on 'remove', @remove

    render: =>
        json = @model.toJSON()
        json.displayAdded = new XDate(json.added).toString('yyyy-MM-dd HH:mm:ss')
        @$el.html @template(json)
        @$('a').attr('href', @model.get 'self')
        if @model.voted
            @disableVoting true
            @hideVoting()
        if !@model.get('deletable')
            @$('button.action-remove').hide()
        return @

    remove: =>
        @$el.remove()

    voteUp: ->
        @disableVoting true
        d = @model.voteUp()
        d.fail (f) =>
            @flash()
            @disableVoting false

    voteDown: ->
        @disableVoting true
        d = @model.voteDown()
        d.fail (f) =>
            @flash()
            @disableVoting false

    actionRemove: ->
        @disableVoting true
        d = @model.destroy()
        d.done (data) =>
            @remove()
        d.fail (f) =>
            @flash()
            @disableVoting false

    flash: (cssClass='error-flash') ->
        el = $('<div />').addClass cssClass
        @$('.quote').append el
        el.fadeIn(200).fadeOut(600).queue ->
            el.remove()
            el.dequeue()

    disableVoting: (disabled=true) ->
        @$('.controls button').prop 'disabled', disabled

    hideVoting: ->
        controls = @$('.controls')
        controls.css opacity: 1
        controls.find('button').fadeOut 200
        controls.find('.voted').fadeIn 200


class QuoteList extends Backbone.Collection
    model: Quote


class QuoteListView extends Backbone.View
    tagName: 'div'
    template: _.template $('#template-quotelist').html()

    initialize: ->
        @model.on 'add', @addOne
        @model.on 'reset', @reset

    addOne: (quote) =>
        view = new QuoteView(model: quote)
        @$('.quotes').append view.render().el

    reset: (collection, options) =>
        @$('.quotes').empty()
        @model.each (quote) => @addOne quote
        if options.prev
            @$('a.prev').attr('href', options.prev)
        else
            @$('a.prev').removeAttr('href')
        if options.next
            @$('a.next').attr('href', options.next)
        else
            @$('a.next').removeAttr('href')

    render: ->
        @$el.html @template()
        return @


class OverviewView extends Backbone.View
    tagName: 'div'

    render: ->
        return @

    remove: =>
        @$el.remove()


class NewQuoteView extends Backbone.View
    tagName: 'div'
    template: _.template $('#template-newquote').html()
    events:
        'submit form': 'addQuote'

    render: =>
        @$el.html @template()
        return @

    addQuote: (event) ->
        event.preventDefault()
        q = content: @$('textarea').val()
        d = $.ajax '/newQuote',
            data: JSON.stringify(q),
            type: 'POST',
            dataType: 'json',
            contentType: 'application/json'
        d.done (data) ->
            history.pushState {}, '', data.self
            Backbone.history.loadUrl()


class AppView extends Backbone.View
    el: $('#app')

    initialize: ->
        $('body').on 'click', 'a.event-routed', (event) ->
            event.preventDefault()
            router.navigate @pathname, trigger: true

    replaceView: (view) ->
        view.render()
        if @currentView
            @currentView.remove()
        @currentView = view
        @$el.empty
        @$el.append view.el


class CircusRouter extends Backbone.Router
    routes:
        '':                  'overview'
        'top/pages/:page':   'top'
        'quotes':            'quotes'
        'quotes/from/:page': 'quotesPage'
        'quotes/:qid':       'quote'
        'newQuote':          'newQuote'

    overview: ->
        view = new OverviewView
        @app.replaceView view

    loadQuotes: (uri, data) ->
        if @app.currentView instanceof QuoteListView
            quoteList = @app.currentView.model
        else
            quoteList = new QuoteList
            view = new QuoteListView(model: quoteList)
            @app.replaceView view
        $.getJSON(uri, data).success (data) ->
            quoteList.reset data.quotes,
                prev: data.prev
                next: data.next

    top: (page) ->
        @loadQuotes '/top/pages/' + page

    quotes: ->
        @loadQuotes '/quotes'

    quotesPage: (page) ->
        @loadQuotes '/quotes/from/' + page

    quote: (qid) ->
        if @app.currentView instanceof QuoteListView
            quoteList = @app.currentView.model
        else
            quoteList = new QuoteList
            view = new QuoteListView(model: quoteList)
            @app.replaceView view
        $.getJSON('/quotes/' + qid).success (data) ->
            quoteList.reset [data]

    newQuote: ->
        @app.replaceView(new NewQuoteView)


$(document).ready ->
    $('.collapse').collapse()
    app = new AppView
    window.router = new CircusRouter
    router.app = app
    Backbone.history.start pushState: true
    $('#search').on 'submit', (event) ->
        event.preventDefault()
        router.loadQuotes '/quotes', $(this).serialize()
