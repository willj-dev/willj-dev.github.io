
const termCardContainer = document.getElementById('term-card-container');

function makeTermElement(term, definition) {
    const termElement = document.createElement('div');
    termElement.className = "term-card";
    termElement.id = `term-$${term}`;

    termElement.innerHTML = definition;

    termCardContainer.append(termElement);
    return termElement;
}

const terms = {
    $for(terms)-$
    '$term$': makeTermElement('$term$', "$definition$"),
    $endfor-$
};

const termAlternatives = {
    $for(terms)-$
    '$term$': '$term$',
    $for(alternatives)-$
    '$alternative$': '$term$',
    $endfor-$
    $endfor-$
};

const ACTIVE_TERM_CLASS = 'active-term';
function setActiveTerm(term) {
    const termElem = terms[term];
    if (!termElem) {
        console.error(`Failed to find term element to set active: $${term}`);
        return;
    }

    const oldClasses = termElem.className.split();
    if (!oldClasses.includes(ACTIVE_TERM_CLASS)) {
        const newClasses = oldClasses.concat([ACTIVE_TERM_CLASS]);
        termElem.className = newClasses.join(' ');
    }
}

function unsetActiveTerm(term) {
    const termElem = terms[term];
    if (!termElem) {
        console.error(`Failed to find term element to unset active: $${term}`);
        return;
    }

    const newClasses = termElem.className.split(/\s+/).filter(tc => tc !== ACTIVE_TERM_CLASS);
    termElem.className = newClasses.join(' ');
}

const PINNED_TERM_CLASS = 'pinned-term';
function togglePinnedTerm(term) {
    const termElem = terms[term];
    if (!termElem) {
        console.error(`Failed to find term element to (un)pin: $${term}`);
        return;
    }

    const oldClasses = termElem.className.split(/\s+/);
    let newClasses;
    if (oldClasses.includes(PINNED_TERM_CLASS)) {
        newClasses = oldClasses.filter(tc => tc !== PINNED_TERM_CLASS)
    } else {
        newClasses = oldClasses.concat([PINNED_TERM_CLASS]);
    }
    termElem.className = newClasses.join(' ');
}

// add hover/click handlers to term links
Array
    .from(document.getElementsByClassName('term-link'))
    .forEach(termLink => {
        const linkedTermAlternative = termLink.textContent.toLowerCase();
        const linkedTerm = termAlternatives[linkedTermAlternative];

        if (!linkedTerm) {
            console.error(`Failed to find actual term for link with text: $${linkedTermAlternative}`);
            return;
        }

        const termElement = terms[linkedTerm];
        if (!termElement) {
            console.error(`Failed to find element for term $${linkedTerm}`);
        }

        termLink.addEventListener('mouseenter', () => setActiveTerm(linkedTerm));
        termLink.addEventListener('mouseleave', () => unsetActiveTerm(linkedTerm));
        termLink.addEventListener('click', () => togglePinnedTerm(linkedTerm));
        termLink.addEventListener('touchstart', () => setActiveTerm(linkedTerm));
        termLink.addEventListener('touchcancel', () => unsetActiveTerm(linkedTerm));
    })
