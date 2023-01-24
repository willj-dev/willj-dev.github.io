
const termCardContainer = document.getElementById('term-card-container');

function makeTermElement(term, definition) {
    const termElement = document.createElement('div');
    termElement.className = "card term-card";
    termElement.id = `term-${term}`;

    termElement.innerHTML = definition;

    const closeTermCardElement = document.createElement('span');
    closeTermCardElement.className = 'close-term-card material-icons';
    closeTermCardElement.innerHTML = 'close';
    closeTermCardElement.addEventListener('click', () => termCardContainer.removeChild(termElement));
    termElement.prepend(closeTermCardElement);

    return termElement;
}

const terms = {
    'person': makeTermElement('person', '<p>A <em>person</em> is any individual who can demonstrate <code>agency</code> and recognize it in other persons.</p>'),
    'agency': makeTermElement('agency', '<p><em>Agency</em> is a <code>person</code>’s ability to freely choose for themselves how to act.</p>'),
    'adult': makeTermElement('adult', '<p>An <em>adult</em> is any human who is able to exercise complete personal <code>agency</code> responsibly and independently.</p>'),
    'child': makeTermElement('child', '<p>A <em>child</em> is any human from birth until they can be considered an <code>adult</code>.</p>'),
    'guardian': makeTermElement('guardian', '<p>A <code>child</code>’s <em>guardian</em> is an <code>adult</code> who is permitted to exercise agency on a child’s behalf.</p><p>By default, both biological parents and all living direct ancestors are a child’s guardians.</p>'),
    'society': makeTermElement('society', '<p>A <em>society</em> is a system of cooperation by mutual consent between individual <code>persons</code>. It may be explicitly defined by <code>laws</code>, or implicitly upheld by its members.</p>'),
    'rights': makeTermElement('rights', '<p><em>Rights</em> define the level of <code>agency</code> a <code>society</code> allows to an individual <code>person</code>.</p>'),
    'just': makeTermElement('just', '<p>A <em>just</em> society is one which guarantees the same <code>rights</code> equally to all <code>persons</code>.</p>'),
    'government': makeTermElement('government', '<p>A <em>government</em> is any organization which is entrusted by a <code>society</code> to:</p><ul><li>support the <code>rights</code> of its <code>citizens</code> by managing shared resources; and</li><li>defend those rights by enacting <code>legitimate punishments</code> upon persons who commit <code>crimes</code>.</li></ul>'),
    'corrupt': makeTermElement('corrupt', '<p>A <em>corrupt</em> <code>government</code> is one which willfully or negligently abuses its power to protect the <code>rights</code> of certain <code>citizens</code> at the expense of others.</p>'),
    'citizen': makeTermElement('citizen', '<p>A <em>citizen</em> of a particular <code>society</code> is a <code>person</code> whose <code>rights</code> are guaranteed by that society’s <code>government</code>, in return for that person’s implicit assent to abide by any of its <code>laws</code>.</p>'),
    'law': makeTermElement('law', '<p>A <em>law</em> is public written documentation published by a <code>government</code> to direct its own activities or the behavior of its <code>citizens</code>.</p>'),
    'due process': makeTermElement('due process', '<p><em>Due process</em> of law is the set of rules and procedures, defined by <code>law</code>, by which a <code>society</code> allows its <code>government</code> to investigate, prove, and punish <code>crimes</code>. This includes at least:</p><ul><li>Universal presumption of innocence until guilt is proven beyond reasonable doubt;</li><li>Universal access to legal representation that provides equal protection before the law;</li><li>Limiting the scope of warranted violation of personal privacy to the extent justifiably necessary for successful prosecution of a crime; and</li><li>Equal and reasonably generous opportunity for the accused to attend their trial, choose their own legal representative, and have their guilt decided by a jury.</li></ul>'),
    'crime': makeTermElement('crime', '<p>A <em>crime</em> is any act, defined by <code>law</code>, by which one <code>person</code> harms or threatens the <code>rights</code> of another out of malice or negligence.</p>'),
    'punishment': makeTermElement('punishment', '<p>A <em>punishment</em> is any act limiting the <code>agency</code> of a <code>person</code> who has committed a <code>crime</code>.</p>'),
    'legitimate punishment': makeTermElement('legitimate punishment', '<p>A punishment is <em>legitimate</em> if it is enacted by a <code>government</code> in response to a <code>crime</code> under the following conditions:</p><ul><li>the crime is proven with <code>due legal process</code> to have been committed against an individual <code>citizen</code> of that government, or to have significantly affected the government’s ability to provide for the <code>rights</code> of its citizens;</li><li>the crime was defined by <code>law</code> at the time it occurred;</li><li>the punishment is the minimum necessary length and severity to ensure the ongoing protection of the rights of all other citizens;</li><li>any fines imposed are fairly representative of the damage done or prevented;</li><li>no punishment may involve avoidable or intentional physical harm; and</li><li>no punishment may involve avoidable emotional harm.</li></ul>'),
    'high-technology': makeTermElement('high-technology', '<p>A <em>high-technology</em> <code>society</code> is one in which there is sufficient industry and automation to guarantee the <code>rights</code> specified in the MSC_ universally and equally, without requiring each <code>person</code> to be actively involved in producing goods or providing services.</p>'),
    'utopian': makeTermElement('utopian', '<p>A <em>Utopian</em> is a <code>person</code> who believes that a truly <code>just</code> society is a practically achievable goal.</p>'),
    'millennial utopian': makeTermElement('millennial utopian', '<p>A <em>Millennial Utopian</em> is a <code>person</code> who believes that human society at the beginning of the 21st century has sufficiently <code>high technological</code> development to successfully implement a <code>just</code> <code>society</code>.</p>'),
    'abduction': makeTermElement('abduction', '<p><em>Abduction</em> is any act by which one <code>person</code> deprives another person of their right to travel freely. This includes but is not limited to slavery, kidnapping, or involuntary confinement.</p>'),
    'health care': makeTermElement('health care', '<p>TBD</p>'),
    'nutrition': makeTermElement('nutrition', '<p>TBD</p>'),
    'sustainable environment': makeTermElement('sustainable environment', '<p>TBD</p>'),
    'residence': makeTermElement('residence', '<p>A <em>residence</em> is a connected indoor area with controlled access to specific <code>persons</code>, the <em>residents</em>. A residence must have at least one private bedroom per resident (unless shared by mutual consent); sufficient toilets, sinks, and showers to provide for the sanitary needs of all residents; and sufficient kitchen space with food storage and dining areas for all residents.</p><p>A residence is <em>short-term</em> if it guarantees access to a particular resident for less than one week, and <em>long-term</em> if it guarantees access to a particular resident for at least one year.</p><p>A residence is <em>safe</em> if it provides a healthy living environment with the least practicable risk of injury to any person inside it.</p>'),
    'basic utilities': makeTermElement('basic utilities', '<p><em>Basic utilities</em> are resources or services available in a <code>residence</code> which may not be limited except as a <code>legitimate punishment</code> in response to excessive consumption or misuse; and which in any case must always provide a reasonable minimum level of service. In contemporary human society, these resources or services must at least include:</p><ul><li>Potable running water (hot and cold)</li><li>Electrical power</li><li>Unrestricted access to public information and communication networks</li><li>Shared and private storage for both physical belongings and personal data</li><li>Climate control appropriate to expected local weather conditions</li></ul>'),
    'identity information': makeTermElement('identity information', '<p><em>Identity information</em> is any information (such as name, address, or date of birth) which identifies a specific <code>person</code> in a population.</p>'),
    'personal data': makeTermElement('personal data', '<p><em>Personal data</em> is any data which can be directly associated with one or more <code>identifiable</code> <code>persons</code>.</p>'),
    };

const termAlternatives = {
    'person': 'person',
    'persons': 'person',
    'agency': 'agency',
    'adult': 'adult',
    'adults': 'adult',
    'child': 'child',
    'children': 'child',
    'guardian': 'guardian',
    'guardians': 'guardian',
    'guardianship': 'guardian',
    'society': 'society',
    'rights': 'rights',
    'just': 'just',
    'government': 'government',
    'governmental': 'government',
    'corrupt': 'corrupt',
    'citizen': 'citizen',
    'citizens': 'citizen',
    'citizenship': 'citizen',
    'law': 'law',
    'laws': 'law',
    'due process': 'due process',
    'due legal process': 'due process',
    'crime': 'crime',
    'crimes': 'crime',
    'punishment': 'punishment',
    'legitimate punishment': 'legitimate punishment',
    'legitimate punishments': 'legitimate punishment',
    'high-technology': 'high-technology',
    'high technological': 'high-technology',
    'utopian': 'utopian',
    'millennial utopian': 'millennial utopian',
    'abduction': 'abduction',
    'health care': 'health care',
    'healthcare': 'health care',
    'nutrition': 'nutrition',
    'sustainable environment': 'sustainable environment',
    'residence': 'residence',
    'residences': 'residence',
    'resident': 'residence',
    'residents': 'residence',
    'basic utilities': 'basic utilities',
    'identity information': 'identity information',
    'identifiable': 'identity information',
    'personal data': 'personal data',
    };

const ACTIVE_TERM_CLASS = 'active-term';
function setActiveTerm(term) {
    const termElem = terms[term];
    if (!termElem) {
        console.error(`Failed to find term element to set active: ${term}`);
        return;
    }

    const oldClasses = termElem.className.split();
    if (!oldClasses.includes(ACTIVE_TERM_CLASS)) {
        const newClasses = oldClasses.concat([ACTIVE_TERM_CLASS]);
        termElem.className = newClasses.join(' ');
    }

    if (!termElem.isConnected) {
        addTermLinkHandlers(termElem);
        termCardContainer.appendChild(termElem);
    }
}

function unsetActiveTerm(term) {
    const termElem = terms[term];
    if (!termElem) {
        console.error(`Failed to find term element to unset active: ${term}`);
        return;
    }

    const newClasses = termElem.className.split(/\s+/).filter(tc => tc !== ACTIVE_TERM_CLASS);
    termElem.className = newClasses.join(' ');

    if (termElem.isConnected && !newClasses.includes(PINNED_TERM_CLASS)) {
        termCardContainer.removeChild(termElem);
    }
}

const PINNED_TERM_CLASS = 'pinned-term';
function togglePinnedTerm(term) {
    const termElem = terms[term];
    if (!termElem) {
        console.error(`Failed to find term element to (un)pin: ${term}`);
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

function addTermLinkHandlers(parentNode) {
    Array
        .from(parentNode.getElementsByClassName('term-link'))
        .forEach(termLink => {
            const linkedTermAlternative = termLink.textContent.toLowerCase();
            const linkedTerm = termAlternatives[linkedTermAlternative];

            if (!linkedTerm) {
                console.error(`Failed to find actual term for link with text: ${linkedTermAlternative}`);
                return;
            }

            const termElement = terms[linkedTerm];
            if (!termElement) {
                console.error(`Failed to find element for term ${linkedTerm}`);
            }

            termLink.addEventListener('mouseenter', () => setActiveTerm(linkedTerm));
            termLink.addEventListener('mouseleave', () => unsetActiveTerm(linkedTerm));
            termLink.addEventListener('click', () => togglePinnedTerm(linkedTerm));
            termLink.addEventListener('touchstart', () => setActiveTerm(linkedTerm));
            termLink.addEventListener('touchcancel', () => unsetActiveTerm(linkedTerm));
        })
}

// add hover/click handlers to term links
addTermLinkHandlers(document);
