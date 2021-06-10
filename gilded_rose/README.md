# GnuCOBOL Gilded Rose

A GnuCOBOL version of the [Gilded Rose Kata](https://github.com/emilybache/GildedRose-Refactoring-Kata).

A slightly modified version of the requirements specification follows.

> ## Gilded Rose Requirements Specification
> 
> Hi and welcome to team Gilded Rose. As you know, we are a small inn with a prime location in a prominent city ran by a friendly innkeeper named Allison. We also buy and sell only the finest goods. Unfortunately, our goods are constantly degrading in quality as they approach their sell by date. We have a system in place that updates our inventory for us. It was developed by a no-nonsense type named Leeroy, who has moved on to new adventures. Your task is to add the new feature to our system so that we can begin selling a new category of items. First an introduction to our system:
> 
> - All items have a SellIn value which denotes the number of days we have to sell the item
> - All items have a Quality value which denotes how valuable the item is
> - At the end of each day our system lowers both values for every item
> 
> Pretty simple, right? Well this is where it gets interesting:
> 
> - Once the sell by date has passed, Quality degrades twice as fast
> - The Quality of an item is never negative
> - "Aged Brie" actually increases in Quality the older it gets
> - The Quality of an item is never more than 50
> - "Sulfuras", being a legendary item, never has to be sold or decreases in Quality
> - "Backstage passes", like aged brie, increases in Quality as its SellIn value approaches;
>   - Quality increases by 2 when there are 10 days or less and by 3 when there are 5 days or less but
>   - Quality drops to 0 after the concert
> 
> We have recently signed a supplier of conjured items. This requires an update to our system:
> 
> - "Conjured" items degrade in Quality twice as fast as normal items (don't worry about multiple rules like 'Conjured Aged Brie' etc)
> 
> Feel free to make any changes to the module and add any new code as long as everything still works correctly. However, do not alter the data format as it is relied upon by the goblin in the corner who will insta-rage and one-shot you as he doesn't believe in shared code ownership.
> 
> Just for clarification, an item can never have its Quality increase above 50, however "Sulfuras" is a legendary item and as such its Quality is 80 and it never alters.

## Setup

To get set up, clone this repo and navigate to this directory, then:

```shell
; ./cbl setup
# You should see one failing test to get you started.
```

And you can then run:

```shell
; ./cbl test # To run the tests
; ./cbl run # To run a sample program
```

See the [gnucobol-starter README](https://github.com/makersacademy/gnucobol-starter/tree/v0.1) for further info.

## License

[The original authorship and license information for the exercise can be found here.](https://github.com/emilybache/GildedRose-Refactoring-Kata).
